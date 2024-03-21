"""dnf plugin to set accesstoken http header for enabled repos"""
import json
import logging
import dnf
import urlgrabber

class AccessToken(dnf.Plugin):
    """dnf accesstoken plugin class"""

    name = "accesstoken"

    def config(self):
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

            if not (token['token'] and token['token_id']):
                raise Exception("Invalid token or token_id")

            access_token = f'X-Access-Token:{str(token["token"])}'
            referer = f'Referer:{str(token["token_id"])}'
            repo.set_http_headers([access_token, referer])
