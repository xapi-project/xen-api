"""dnf plugin to set xapitoken http header for enabled repos"""
import json
import logging
# Disable the error, it can be import in production env
# and mocked out in unitttest
# pylint: disable=import-error
# pytype: disable=import-error
import dnf
import urlgrabber


class InvalidToken(Exception):
    """Token is invalid"""
    def __init__(self, token):
        super().__init__(f"Invalid token: {token}")


#pylint: disable=too-few-public-methods
class XapiToken(dnf.Plugin):
    """dnf xapitoken plugin class"""

    name = "xapitoken"

    def config(self):
        """ DNF plugin config hook,
        refer to https://dnf.readthedocs.io/en/latest/api_plugins.html"""

        for repo_name in self.base.repos:
            repo = self.base.repos[repo_name]

            token_url = repo.xapitoken
            if not token_url or token_url == '':
                continue
            try:
                token_str = urlgrabber.urlopen(token_url).read().strip()
                token = json.loads(token_str)
            except Exception:  #pylint: disable=broad-except
                logging.debug("Failed to load token from: %s", token_url)
                continue

            if not token.get('xapitoken'):
                raise InvalidToken(token)

            # Only include the xapitoken for repos with a localhost URL, for added safety.
            # These will be proxied to the remote pool coordinator through stunnel, set up by xapi.
            if len(repo.baseurl) > 0 and repo.baseurl[0].startswith("http://127.0.0.1") \
                and repo.xapitoken:
                secret = "session_id=" + str(token["xapitoken"])
                repo.set_http_headers([f'cookie:{secret}'])
