#!/bin/sh

set -e
set -x

automake --add-missing --copy || true
aclocal -I m4
autoconf
