"""dnf plugin to add ptoken for repos"""
import dnf

class Ptoken(dnf.Plugin):
    """ptoken plugin class"""
    name = "ptoken"

    def config(self):
        with open('/etc/xensource/ptoken', encoding="utf-8") as file:
            ptoken = file.read().strip()

        for repo_name in self.base.repos:
            repo = self.base.repos[repo_name]

            if len(repo.baseurl) > 0 and repo.baseurl[0].startswith("http://127.0.0.1") \
                and repo.ptoken:
                secret = "pool_secret=" + ptoken
                repo.set_http_headers([f'cookie:{secret}'])
