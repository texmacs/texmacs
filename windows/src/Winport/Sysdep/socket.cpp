#include <winsock2.h>
#include <sys/socket.h>
#include <map>
using namespace std;

#pragma warning(disable: 4786)

typedef struct SOCKET_STRUCT_{
	SOCKET sock;
	bool blocking;
}SOCKET_STRUCT;

map<SOCKET,SOCKET_STRUCT> socketMap;

SOCKET_STRUCT* GetSocketStruct(SOCKET sock);

int SOCKET_connect(int s, const struct sockaddr * name, int namelen){

	return connect(s, name, namelen);
}

u_short SOCKET_htons(u_short hostshort){

	return htons(hostshort);
}

struct hostent* SOCKET_gethostbyname(const char* name){

	return gethostbyname(name);
}

bool SOCKET_IsSocket(int des){

	return GetSocketStruct(des) != NULL;
}

int SOCKET_socket(int af, int type, int protocol){

	SOCKET sock;
	SOCKET_STRUCT sockStruct;

	sock = socket(af, type, protocol);

	if(sock == INVALID_SOCKET)
		return -1;

	sockStruct.sock = sock;
	sockStruct.blocking = false;

	socketMap[sock] = sockStruct;
	return sock;
}

int SOCKET_fcntl(int fd, int cmd, long arg){

	SOCKET_STRUCT *ptr = GetSocketStruct(fd);

	if(ptr == NULL)
		return -1;

	if(cmd == F_SETFL){
		if(cmd == O_NONBLOCK){
			ptr->blocking = false;
			return 0;
		}
	}

	return -1;
}

SOCKET_STRUCT* GetSocketStruct(SOCKET sock){

	map<SOCKET,SOCKET_STRUCT>::iterator iter;

	iter = socketMap.find(sock);

	if(iter == socketMap.end())
		return NULL;
	
	return &iter->second;
}

int SOCKET_write(int d, const void *buf, size_t nbytes){

	SOCKET_STRUCT *ptr = GetSocketStruct(d);
	TIMEVAL time;
	int numBytes;

	time.tv_sec = 0;
	time.tv_usec = 0;

	if(!ptr){
	
		return ERROR; 
	}

	if(ptr->blocking){
		fd_set set;
		set.fd_count = 1;
		set.fd_array[0] = d;

		if(select(0, NULL, &set, NULL, &time) == 0){
			return 0;
		}
	}
	
	numBytes = send(d, (char*)buf, nbytes, 0);
		
	if(numBytes == SOCKET_ERROR)
		return ERROR;

	return numBytes;
	
}

int SOCKET_read(int d, void *buf, size_t nbytes){

	SOCKET_STRUCT *ptr = GetSocketStruct(d);
	TIMEVAL time;
	int numBytes;

	time.tv_sec = 0;
	time.tv_usec = 0;

	if(!ptr){
		return ERROR; 
	}

	if(ptr->blocking){
		fd_set set;
		set.fd_count = 1;
		set.fd_array[0] = d;

		if(select(0, &set, NULL, NULL, &time) == 0){
			return 0;
		}
	}
	
	numBytes = recv(d, (char*)buf, nbytes, 0);
		
	if(numBytes == SOCKET_ERROR)
		return ERROR;

	return numBytes;
}

int SOCKET_close(int d){

	closesocket(d);

	socketMap.erase(d);

	return 0;
}

int SOCKET__WSAFDIsSet(unsigned int uint, fd_set *set){

	return __WSAFDIsSet(uint, set);
}