/*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * 
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections.Generic;
using System.Text;

namespace XenAPI
{
    public class UserDetails
    {
        /// <summary>
        /// Mapping of SIDS to UserDetails.
        /// </summary>
        private static Dictionary<string, UserDetails> sid_To_UserDetails = new Dictionary<string, UserDetails>();
        public static void UpdateDetails(string SID, Session session)
        {
            lock (UserDetails.sid_To_UserDetails)
            {
                UserDetails.sid_To_UserDetails.Remove(SID);
                UserDetails.sid_To_UserDetails.Add(SID, new UserDetails(session));
            }
        }
        public static Dictionary<string, UserDetails> Sid_To_UserDetails
        {
            get
            {
                lock (UserDetails.sid_To_UserDetails)
                {
                    return sid_To_UserDetails;
                }
            }
        }

        private string userSid = null;
        private string userDisplayName = null;
        private string userName = null;
        private string[] groupMembershipNames = null;
        private string[] groupMembershipSids = null;
        private readonly Session _session;

        /// <summary>
        /// The Active Directory SID of this subject. 
        /// </summary>
        public string UserSid { get { return userSid; } }

        /// <summary>
        /// The Active Directory DisplayName of the subject.
        /// Null if the lookup failed.
        /// </summary>
        public string UserDisplayName { get { return userDisplayName; } }

        /// <summary>
        /// The Active Directory Name of the subject.
        /// Null if the lookup failed.
        /// </summary>
        public string UserName { get { return userName; } }

        /// <summary>
        /// The Active Directory group names the subject belongs to.
        /// </summary>
        public string[] GroupMembershipNames
        {
            get { return groupMembershipNames ?? (groupMembershipNames = GetGroupMembershipNames(_session)); }
        }

        /// <summary>
        /// The Active Directory group sids the subject belongs to.
        /// </summary>
        public string[] GroupMembershipSids { get { return groupMembershipSids; } }

        /// <summary>
        /// Makes server calls, call off the event thread.
        /// </summary>
        /// <param name="session"></param>
        private UserDetails(Session session)
        {
            _session = session;
            userSid = session.UserSid;

            try
            {
                Subject subj = new Subject();
                subj.other_config = Auth.get_subject_information_from_identifier(session, userSid);
                userDisplayName = subj.DisplayName;
                userName = subj.SubjectName;
                groupMembershipSids = Auth.get_group_membership(session, userSid);
            }
            catch(Failure)
            {
            }
        }

        /// <summary>
        /// Gets Active Directory group names the subject belongs to.
        /// Makes server calls. This could take some time for very large group memberships.
        /// </summary>
        private string[] GetGroupMembershipNames(Session session)
        {
            try
            {
                if (groupMembershipSids != null)
                {
                    var output = new string[groupMembershipSids.Length];

                    for (int i = 0; i < groupMembershipSids.Length; i++)
                    {
                        string sid = groupMembershipSids[i];
                        Dictionary<String, String> info = Auth.get_subject_information_from_identifier(session, sid);
                        string name = "";

                        if (info.TryGetValue("subject-displayname", out name))
                        {
                            output[i] = name;
                            continue;
                        }
                        if (info.TryGetValue("subject-name", out name))
                        {
                            output[i] = name;
                            continue;
                        }

                        output[i] = sid;
                    }
                    return output;
                }
            }
            catch (Failure)
            {
            }
            return null;
        }
    }
}
