#ifndef SOCKET_H
#define SOCKET_H

#include <sys/_types.h>

#ifdef _WINSOCKAPI_
#else
#ifdef _WINSOCK2API_
#else
#ifdef _WINBASE2_
#else
#ifdef _WINDOWS2_
#else

#define AF_UNIX         1               /* local to host (pipes, portals) */
#define AF_INET         2               /* internetwork: UDP, TCP, etc. */
#define AF_IMPLINK      3               /* arpanet imp addresses */
#define AF_PUP          4               /* pup protocols: e.g. BSP */
#define AF_CHAOS        5               /* mit CHAOS protocols */
#define AF_NS           6               /* XEROX NS protocols */
#define AF_IPX          AF_NS           /* IPX protocols: IPX, SPX, etc. */
#define AF_ISO          7               /* ISO protocols */
#define AF_OSI          AF_ISO          /* OSI is ISO */
#define AF_ECMA         8               /* european computer manufacturers */
#define AF_DATAKIT      9               /* datakit protocols */
#define AF_CCITT        10              /* CCITT protocols, X.25 etc */
#define AF_SNA          11              /* IBM SNA */
#define AF_DECnet       12              /* DECnet */
#define AF_DLI          13              /* Direct data link interface */
#define AF_LAT          14              /* LAT */
#define AF_HYLINK       15              /* NSC Hyperchannel */
#define AF_APPLETALK    16              /* AppleTalk */
#define AF_NETBIOS      17              /* NetBios-style addresses */
#define AF_VOICEVIEW    18              /* VoiceView */
#define AF_FIREFOX      19              /* Protocols from Firefox */
#define AF_UNKNOWN1     20              /* Somebody is using this! */
#define AF_BAN          21              /* Banyan */
#define AF_ATM          22              /* Native ATM Services */
#define AF_INET6        23              /* Internetwork Version 6 */
#define AF_CLUSTER      24              /* Microsoft Wolfpack */
#define AF_12844        25              /* IEEE 1284.4 WG AF */

#define SOCK_STREAM     1               /* stream socket */
#define SOCK_DGRAM      2               /* datagram socket */
#define SOCK_RAW        3               /* raw-protocol interface */
#define SOCK_RDM        4               /* reliably-delivered message */
#define SOCK_SEQPACKET  5               /* sequenced packet stream */

struct in_addr {
        union {
                struct { u_char s_b1,s_b2,s_b3,s_b4; } S_un_b;
                struct { u_short s_w1,s_w2; } S_un_w;
                u_long S_addr;
        } S_un;
};

struct sockaddr_in {
        short   sin_family;
        u_short sin_port;
        struct  in_addr sin_addr;
        char    sin_zero[8];
};

struct  hostent {
        char    * h_name;           /* official name of host */
        char    ** h_aliases;  /* alias list */
        short   h_addrtype;             /* host address type */
        short   h_length;               /* length of address */
        char    ** h_addr_list; /* list of addresses */
#define h_addr  h_addr_list[0]          /* address, for backward compat */
};

struct timeval{
	unsigned long tv_sec;
	unsigned long tv_usec; 
};

#define FD_SETSIZE      64

typedef struct fd_set {
        u_int fd_count;               /* how many are SET? */
        int  fd_array[FD_SETSIZE];   /* an array of SOCKETs */
} fd_set;

#define FD_CLR(fd, set) do { \
    u_int __i; \
    for (__i = 0; __i < ((fd_set *)(set))->fd_count ; __i++) { \
        if (((fd_set *)(set))->fd_array[__i] == fd) { \
            while (__i < ((fd_set *)(set))->fd_count-1) { \
                ((fd_set *)(set))->fd_array[__i] = \
                    ((fd_set *)(set))->fd_array[__i+1]; \
                __i++; \
            } \
            ((fd_set *)(set))->fd_count--; \
            break; \
        } \
    } \
} while(0)

#define FD_SET(fd, set) do { \
    u_int __i; \
    for (__i = 0; __i < ((fd_set *)(set))->fd_count; __i++) { \
        if (((fd_set *)(set))->fd_array[__i] == (fd)) { \
            break; \
        } \
    } \
    if (__i == ((fd_set *)(set))->fd_count) { \
        if (((fd_set *)(set))->fd_count < FD_SETSIZE) { \
            ((fd_set *)(set))->fd_array[__i] = (fd); \
            ((fd_set *)(set))->fd_count++; \
        } \
    } \
} while(0)

#define FD_ZERO(set) (((fd_set *)(set))->fd_count=0)

int SOCKET__WSAFDIsSet(unsigned int, fd_set *);

#define FD_ISSET(fd, set) SOCKET__WSAFDIsSet((int)(fd), (fd_set *)(set))

#endif
#endif
#endif
#endif

#define O_NONBLOCK        04000
#define F_SETFL         4 


bool SOCKET_IsSocket(int des);
int SOCKET_fcntl(int fd, int cmd, long arg);
int SOCKET_write(int d, const void *buf, size_t nbytes); 
int SOCKET_read(int d, void *buf, size_t nbytes); 
int SOCKET_close(int d); 
int SOCKET_socket(int af, int type, int protocol);
struct hostent* SOCKET_gethostbyname(const char* name);
u_short SOCKET_htons(u_short hostshort);
int SOCKET_connect(int s, const struct sockaddr * name, int namelen);

#endif