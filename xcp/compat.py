"""Helper module for setting up binary or UTF-8 I/O for Popen and open in Python 3.6 and newer"""
# pyright: strict
# pyright: reportTypeCommentUsage=false
# pyright: reportUnknownParameterType=false
# pytype: disable=bad-return-type,ignored-type-comment,invalid-function-type-comment
# See README-Unicode.md for Details
import sys
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import IO, Any  # pylint: disable=unused-import  # pragma: no cover

def open_textfile(filename, mode, encoding="utf-8", **kwargs):
    # type:(str, str, str, Any) -> IO[str]
    """For best type checking and type inference, wrap open_with_codec_handling returning IO[str]"""
    if "b" in mode:
        raise ValueError("open_textfile returns IO[str]: mode must not contain 'b'")
    return open_with_codec_handling(filename, mode, encoding, **kwargs)

# pylint: disable=unspecified-encoding
if sys.version_info >= (3, 0):
    open_utf8 = {"encoding": "utf-8", "errors": "replace"}

    def open_with_codec_handling(filename, mode="r", encoding="utf-8", **kwargs):
        # type:(str, str, str, Any) -> IO[Any]
        """Helper for open(): Handle UTF-8: Default to encoding="utf-8", errors="replace" for Py3"""
        if "b" in mode:
            # Binary mode: just call open() unmodified:
            return open(filename, mode, **kwargs)  # pragma: no cover
        # Text mode: default to UTF-8 with error handling to replace malformed UTF-8 sequences
        # Needed for Python 3.6 when no UTF-8 locale is set:
        kwargs.setdefault("encoding", encoding)
        kwargs.setdefault("errors", "replace")  # Simple codec error handler: Replace malformed char
        return open(filename, mode, **kwargs)  # type: ignore[call-overload]

else:
    open_utf8 = {}

    def open_with_codec_handling(filename, mode="r", encoding="", errors="", **kwargs):
        # type:(str, str, str, str, str) -> IO[Any]
        """open() wrapper to pass mode and **kwargs to open(), ignores endcoding and errors args"""
        _ = encoding
        _ = errors
        return open(filename, mode, **kwargs)


def open_defaults_for_utf8_text(args, kwargs):
    # type:(tuple[Any, ...] | None, Any) -> tuple[str, Any]
    """Setup keyword arguments for UTF-8 text mode with codec error handler to replace chars"""
    other_kwargs = kwargs.copy()
    mode = other_kwargs.pop("mode", "")
    if args:
        mode = args[0]
    if sys.version_info < (3, 0):
        return mode, other_kwargs
    if sys.version_info >= (3, 0) and "b" not in mode:
        kwargs.setdefault("encoding", "utf-8")
        kwargs.setdefault("errors", "replace")
    return mode or "r", other_kwargs
