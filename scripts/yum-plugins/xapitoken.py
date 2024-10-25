#!/usr/bin/python

# Drop this file into /usr/lib/yum-plugins/
# Enable it by creating conf file /etc/yum/pluginconf.d/xapitoken.conf:
#   [main]
#   enabled=1
#
# Configure it by:
# yum-config-manager --setopt=<repo-name>.xapitoken=file://<token-file-path> --save

# The content of the file referred by the <token-file-path> looks like:
# { 'xapitoken': '...' }

import json
from yum import config
from yum.plugins import TYPE_CORE
import urlgrabber


requires_api_version = '2.5'
plugin_type = (TYPE_CORE,)

def config_hook(conduit):  # pylint: disable=unused-argument
    config.RepoConf.xapitoken = config.UrlOption()

def init_hook(conduit):
    repos = conduit.getRepos()
    for name in repos.repos:
        repo = repos.repos[name]
        token_url = repo.getConfigOption('xapitoken')
        if not token_url or token_url == '':
            continue

        token = {}
        try:
            token_str = urlgrabber.urlopen(token_url).read().strip()
            token = json.loads(token_str)
        except Exception:  #pylint: disable=broad-except
            continue

        if not token['xapitoken']:
            raise Exception("Invalid xapitoken")  #pylint: disable=broad-exception-raised

        # Only include the xapitoken for repos with a localhost URL, for added safety.
        # These will be proxied to the remote pool coordinator through stunnel, set up by xapi.
        if len(repo.baseurl) > 0 and repo.baseurl[0].startswith("http://127.0.0.1") \
            and repo.getConfigOption('xapitoken'):
            repo.http_headers['cookie'] = "session_id=" + str(token['xapitoken'])
