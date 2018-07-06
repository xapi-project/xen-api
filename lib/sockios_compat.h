/* Our dom0 chroot doesn't include up to date headers: */

#define SIOCBRADDBR     0x89a0          /* create new bridge device     */
#define SIOCBRDELBR     0x89a1          /* remove bridge device         */
#define SIOCBRADDIF     0x89a2          /* add interface to bridge      */
#define SIOCBRDELIF     0x89a3          /* remove interface from bridge */
