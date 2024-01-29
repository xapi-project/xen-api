"""scripts/unit_test/conftest.py: Common pytest module for shared pytest fixtures"""
import pytest

from .rootless_container import enter_private_mount_namespace


@pytest.fixture(scope="session")
def private_mount_namespace():
    """Enter a private mount namespace that allows us to test mount and unmount"""
    return enter_private_mount_namespace()
