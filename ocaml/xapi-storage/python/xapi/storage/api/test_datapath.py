from .datapath import datapath_server_test


def internal_error(error):
    """Return a dictionary with an internal error"""
    return {"ErrorDescription": ["Internal_error", error], "Status": "Failure"}


def assert_error(testee, caplog, method_args, method, error):
    """Assert that the result of the testee matches the expected error result"""
    args = method_args.copy()
    if method != "open":  # the persistent arg is only checked for the open method
        args["persistent"] = None  # pass it, but with a wrong type(not used/checked)
    assert testee._dispatch(f"Datapath.{method}", [args]) == internal_error(error)
    assert caplog.messages[0] == "caught " + error
    caplog.clear()


def assert_type_checks(testee, methods, template_args, bad_args, caplog):
    """Assert that the result of the testee matches the expected result"""
    for arg in bad_args:
        # Sigh, if Python would be strongly typed, we wouldn't need this:
        # Assert the type checks of the arguments
        expected = "bool" if arg == "persistent" else "string"
        other_type = False if expected == "string" else "str"
        for actual in [None, [], (), {"dict": "val"}, 1, 1.0, str, caplog, other_type]:
            bad_args = template_args.copy()
            bad_args[arg] = actual
            error_msg = f"TypeError expected={expected} actual={repr(actual)}"
            for method in methods:
                assert_error(testee, caplog, bad_args, method, error_msg)

        # Remove the argument and assert the missing argument checks
        bad_args.pop(arg)
        error_msg = f"UnmarshalException thing=argument missing ty={arg} desc="
        for method in methods:
            assert_error(testee, caplog, bad_args, method, error_msg)


def test_dispatcher(caplog, capsys):
    """
    Test the dispatcher of the Xapi storage API datapath interface

    The dispatcher is a class that routes the calls to the corresponding methods
    of a given Datapath implementation class.
    """

    # Setup

    # The testee passes them to the Datapath_test class and its attach method
    # is expected to return the values which we use to test the dispatcher:
    args = {"dbg": "", "uri": "uri", "domain": "uuid", "persistent": True}

    # Call

    # datapath_server_test() returns an instance of the dispatcher class that
    # routes the calls to the corresponding methods of the Datapath_test class:
    testee = datapath_server_test()

    # Test the argument checks of the dispatcher to identify missing arguments:

    # Assert type checks on the dbg and uri arguments
    missing = ["dbg", "uri"]
    methods = ["attach", "activate", "deactivate", "detach", "open", "close"]
    assert_type_checks(testee, methods, args, missing, caplog)

    # Assert type checks on the missing domain argument
    missing = ["domain"]
    methods = ["attach", "activate", "deactivate", "detach"]
    assert_type_checks(testee, methods, args, missing, caplog)

    # Assert type checks on the persistent flag for the open method
    missing = ["persistent"]
    methods = ["open"]
    assert_type_checks(testee, methods, args, missing, caplog)

    # BUG: Datapath_test.attach() currently returns an mismatching dictionary:
    # The dispatcher expects a dict with a "domain_uuid" key, but the implementation
    # Datapath_test.attach() returns a dict with a "backend" key instead.
    # Changing the implementation of Datapath_test.attach() will fix this issue.

    # This WOULD be an example expected result, BUT the implementation of
    # Datapath_test.attach() returns an invalid dictionary to the dispatcher:
    assert testee._dispatch("Datapath.attach", [args]) != {
        "Status": "Success",
        "Value": {"domain_uuid": "uuid", "implementation": ("uri", "dbg")},
    }

    # BUG: This is the internal error that Datapath_test.attach() currently triggers:
    assert testee._dispatch("Datapath.attach", [args]) == {
        "ErrorDescription": ["Internal_error", "'domain_uuid'"],
        "Status": "Failure",
    }
    assert caplog.messages[0] == "caught 'domain_uuid'"
    caplog.clear()

    # The other methods work as expected. Setup, Call, Assert:
    success = {"Status": "Success", "Value": {}}
    assert testee._dispatch("Datapath.open", [args]) == success
    assert testee._dispatch("Datapath.activate", [args]) == success
    assert testee._dispatch("Datapath.deactivate", [args]) == success
    assert testee._dispatch("Datapath.detach", [args]) == success
    assert testee._dispatch("Datapath.close", [args]) == success

    # Assert that no errors were logged and no output was printed:
    assert caplog.messages == []  # No messages were logged
    assert capsys.readouterr().out == ""  # No output was printed
    assert capsys.readouterr().err == ""  # No errors were printed
