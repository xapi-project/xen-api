#ifndef _XA_AUTH_H_
#define _XA_AUTH_H_

#define XA_SUCCESS 0
#define XA_ERR_EXTERNAL 1

extern int XA_mh_authorize (const char *username, const char *password, 
			    const char **error);

extern int XA_mh_chpasswd (const char *username, const char *new_passwd, 
			   const char **error);

#endif /* _XA_AUTH_H_ */
