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
        // Very large group memberships cause us to hang on connection time as the get subject info call can take some time.
        private static readonly int MAX_GROUP_LOOKUP = 40;

        /// <summary>
        /// Mapping of SIDS to UserDetails.
        /// </summary>
        private static Dictionary<string, UserDetails> sid_To_UserDetails = new Dictionary<string,UserDetails>();
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
        public string[] GroupMembershipNames { get { return groupMembershipNames; } }

        /// <summary>
        /// The Active Directory group sids the subject belongs to.
        /// </summary>
        public string[] GroupMembershipSids { get { return groupMembershipSids; } }

        /// <summary>
        /// Makes server calls, call off the event thread.
        /// </summary>
        /// <param name="session"></param>
        /// <param name="SID"></param>
        private UserDetails(Session session)
        {
            userSid = session.UserSid;
            userDisplayName = GetDisplayName(session);
            userName = GetName(session);
            GetGroupMembership(session);
        }

        private void GetGroupMembership(Session session)
        {
            try
            {
                groupMembershipSids = Auth.get_group_membership(session, userSid);

                if (groupMembershipSids.Length > MAX_GROUP_LOOKUP)
                    return;

                string[] output = new string[groupMembershipSids.Length];


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
                groupMembershipNames = output;
            }
            catch (Failure)
            {
            }
        }

        private string GetDisplayName(Session session)
        {
            try
            {
                Subject subj = new Subject();
                subj.other_config = Auth.get_subject_information_from_identifier(session, userSid);
                return subj.DisplayName;
            }
            catch (Failure)
            {
                return null;
            }
        }

        private string GetName(Session session)
        {
            try
            {
                Subject subj = new Subject();
                subj.other_config = Auth.get_subject_information_from_identifier(session, userSid);
                return subj.SubjectName;
            }
            catch (Failure)
            {
                return null;
            }
        }
    }
}
