"""This is a stub module for dnf.base

This module is introduced to decouple the dependencies from dnf
modules during unittest, as the github CI ubuntu container has
issues with the python-dnf and python3.11
"""
#pylint: disable=too-few-public-methods
class Plugin:
    """Dnf plugin interface"""
    def __init__(self, base, cli):
        self.base = base
        self.cli = cli
