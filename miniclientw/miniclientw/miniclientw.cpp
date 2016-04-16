#define _WINSOCK_DEPRECATED_NO_WARNINGS
#include <winsock2.h>
#include <Windows.h>

#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>

using std::wstring;
using std::string;
using std::vector;

class Client_error
{
public:
	Client_error(wstring message) : _message {std::move(message)} { }
	wstring message() { return _message; }
private:
	wstring _message;
};

struct Server_info
{
	char host[64];
	char port_str[16];
	int port;
	char pid[16];
	char auth[128];
};

class WSA_context
{
public:
	WSA_context()
	{
		WSADATA wsaData;
		if (WSAStartup(MAKEWORD(2, 2), &wsaData)) {
			throw Client_error(L"WSAStartup failed");
		}
	}
	~WSA_context()
	{
		WSACleanup();
	}
};

class Emacs_socket
{
public:
	Emacs_socket(const char * host, int port)
	{
		_s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

		sockaddr_in addr = {};
		addr.sin_family = AF_INET;
		addr.sin_addr.s_addr = inet_addr(host);
		addr.sin_port = htons(port);

		auto result = connect(_s, (PSOCKADDR)&addr, sizeof(addr));
		if (result == SOCKET_ERROR)	{
			throw Client_error(L"Failed to connect to Emacs");
		}
	}
	Emacs_socket(const Emacs_socket&) = delete;

	~Emacs_socket()
	{
		closesocket(_s);
	}

	void sendall(const char * bytes, size_t size)
	{
		size_t total_sent = 0;
		while (total_sent < size) {
			int sent = send(_s, bytes + total_sent, (int) (size - total_sent), 0);
			if (sent <= 0) {
				throw Client_error(L"send() error");
			}
			total_sent += sent;
		}
	}
private:
	SOCKET _s;
};

vector<wstring> get_argv()
{
	int argc = 0;
	PWSTR * argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	vector<wstring> result(argc);
	std::copy(argv, argv + argc, begin(result));
	LocalFree(argv);
	return result;
}

wstring expand_env_strings(wstring src)
{
	wchar_t dst[1024] = {};
	const auto result = ExpandEnvironmentStringsW(src.c_str(), dst, _countof(dst));
	if (result == 0 || result > _countof(dst)) {
		throw Client_error(L"ExpandEnvironmentStringsW failed");
	}
	return dst;
}

bool does_file_exist(const wstring& path)
{
	std::ifstream file(path, std::ios::in);
	return file.good();
}

wstring server_quote_arg(const wstring& unquoted)
{
	std::wstringstream quoted;
	for (auto c : unquoted) {
		switch (c)
		{
		case ' ':
			quoted << L"&_";
			break;
		case '-':
		case '&':
		case '\n':
			quoted << '&' << c;
			break;
		default:
			quoted << c;
		}
	}
	return quoted.str();
}

string utf8_from_wstring(const wstring& wide)
{
	char utf8[1024] = {};

	auto result = WideCharToMultiByte(
		CP_UTF8, 0,
		wide.data(), (int) wide.size(),
		utf8, (int) sizeof(utf8) - 1,
		NULL, NULL);
	if (!result)
	{
		throw Client_error(L"WideCharToMultiByte failed");
	}
	return utf8;
}

Server_info parse_server_file()
{
	auto path = expand_env_strings(L"%appdata%\\.emacs.d\\server\\server");
	if (!does_file_exist(path))
	{
		path = expand_env_strings(L"%userprofile%\\.emacs.d\\server\\server");
		if (!does_file_exist(path))
		{
			throw Client_error(L"No Emacs server file found");
		}
	}
	
	std::ifstream file(path, std::ios::in);
	Server_info server_info = {};
	file.getline(server_info.host, sizeof(server_info.host), ':');
	file.getline(server_info.port_str, sizeof(server_info.port_str), ' ');
	file.getline(server_info.pid, sizeof(server_info.pid));
	file.getline(server_info.auth, sizeof(server_info.auth));

	server_info.port = atoi(server_info.port_str);

	return server_info;
}

string get_server_message(const char * auth, vector<wstring>::const_iterator files, vector<wstring>::const_iterator end)
{
	std::stringstream out;
	out << "-auth " << auth;
	out << " -current-frame";
	out << " -nowait";
	std::for_each(files, end, [&] (const wstring& file) {
		out << " -file " << utf8_from_wstring(server_quote_arg(file));
	});
	out << "\n";
	return out.str();
}

void run()
{
	const auto argv = get_argv();
	if (argv.size() <= 1)
	{
		throw Client_error(L"No filename provided");
	}
	const auto server = parse_server_file();
	WSA_context wsa;
	Emacs_socket socket(server.host, server.port);
	string message = get_server_message(server.auth, begin(argv) + 1, end(argv));
	socket.sendall(message.data(), message.size());
}

int WINAPI WinMain(HINSTANCE, HINSTANCE, PSTR, int)
{
	try
	{
		run();
	}
	catch (Client_error &err)
	{
		MessageBoxW(NULL, err.message().c_str(), L"Error", MB_OK);
	}
	return 0;
}
