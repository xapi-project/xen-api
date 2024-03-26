"""dnf plugin to set accesstoken http header for enabled repos"""
import json
import logging
# Disable the error, it can be import in production env
# and mocked out in unitttest
#pylint: disable=import-error
import dnf
import urlgrabber


class InvalidToken(Exception):
    """Token is invlaid"""
    def __init__(self, token):
        super().__init__(f"Invalid token: {token}")


#pylint: disable=too-few-public-methods
class AccessToken(dnf.Plugin):
    """dnf accesstoken plugin class"""

    name = "accesstoken"

    def config(self):
        """ DNF plugin config hook,
        refer to https://dnf.readthedocs.io/en/latest/api_plugins.html"""

        for repo_name in self.base.repos:
            repo = self.base.repos[repo_name]

            token_url = repo.accesstoken
            if not token_url or token_url == '':
                continue
            try:
                token_str = urlgrabber.urlopen(token_url).read().strip()
                token = json.loads(token_str)
            except Exception:  #pylint: disable=broad-except
                logging.debug("Failed to load token from: %s", token_url)
                continue

            if not (token.get('token') and token.get('token_id')):
                raise InvalidToken(token)

            access_token = f'X-Access-Token:{str(token["token"])}'
            referer = f'Referer:{str(token["token_id"])}'
            repo.set_http_headers([access_token, referer])
