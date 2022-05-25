#!/usr/bin/python

# Drop this file into /usr/lib/yum-plugins/
# Enable it by creating conf file /etc/yum/pluginconf.d/accesstoken.conf:
#   [main]
#   enabled=1
#
# Configure it by:
# yum-config-manager --setopt=<repo-name>.accesstoken=file://<token-file-path> --save

# The content of the file referred by the <token-file-path> looks like:
# { 'token': '...', 'token_id': '...' }

from yum import config
from yum.plugins import TYPE_CORE
import json
import urlgrabber


requires_api_version = '2.5'
plugin_type = (TYPE_CORE,)

def config_hook(conduit):
    config.RepoConf.accesstoken = config.UrlOption()

def init_hook(conduit):
    repos = conduit.getRepos()
    for name in repos.repos:
        repo = repos.repos[name]
        token_url = repo.getConfigOption('accesstoken')
        if not token_url or token_url == '':
            continue

        token = {}
        try:
            token_str = urlgrabber.urlopen(token_url).read().strip()
            token = json.loads(token_str)
        except:
            continue

        if not (token['token'] and token['token_id']):
            raise Exception("Invalid token or token_id")

        repo.http_headers['X-Access-Token'] = str(token['token'])
        repo.http_headers['Referer'] = str(token['token_id'])
