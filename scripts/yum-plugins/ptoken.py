#!/usr/bin/python

# Drop this file into /usr/lib/yum-plugins/
# Enable it by creating conf file /etc/yum/pluginconf.d/ptoken.conf:
#   [main]
#   enabled=1
#
# Configure it by:
# yum-config-manager --setopt=<repo-name>.ptoken=file://<token-file-path> --save

from yum import config
from yum.plugins import TYPE_CORE

requires_api_version = '2.5'
plugin_type = (TYPE_CORE,)

def config_hook(conduit):
    config.RepoConf.ptoken = config.BoolOption(False)

def init_hook(conduit):
    with open('/etc/xensource/ptoken') as f:
        ptoken = f.read().strip()

    repos = conduit.getRepos()
    for name in repos.repos:
        repo = repos.repos[name]
        # Only include the ptoken for repos with a localhost URL, for added safety.
        # These may be proxied to the coordinator through stunnel, set up by xapi.
        if len(repo.baseurl) > 0 and repo.baseurl[0].startswith("http://127.0.0.1") \
            and repo.getConfigOption('ptoken'):
            repo.http_headers['cookie'] = "pool_secret=" + ptoken
