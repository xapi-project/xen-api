"""dnf plugin to add ptoken for repos"""
import logging
import dnf

PTOKEN_PATH = "/etc/xensource/ptoken"

#pylint: disable=too-few-public-methods
class Ptoken(dnf.Plugin):
    """ptoken plugin class"""

    name = "ptoken"

    def config(self):
        """ DNF plugin config hook,
        refer to https://dnf.readthedocs.io/en/latest/api_plugins.html"""
        try:
            with open('/etc/xensource/ptoken', encoding="utf-8") as file:
                ptoken = file.read().strip()
        except Exception: #pylint: disable=broad-exception-caught
            logging.error("Failed to open %s", PTOKEN_PATH)
            raise

        for repo_name in self.base.repos:
            repo = self.base.repos[repo_name]

            if len(repo.baseurl) > 0 and repo.baseurl[0].startswith("http://127.0.0.1") \
                and repo.ptoken:
                secret = "pool_secret=" + ptoken
                repo.set_http_headers([f'cookie:{secret}'])
