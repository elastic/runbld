exec > /dev/null 2>&1

# see if the current directory ends with the path that was passed in
pwd | grep "/some/other/dir$"
if [ $? -eq 0 ]; then
    exit 0
else
    # probably not a problem to return whatever grep returns, but just
    # in case, munge all failures into 1
    exit 1
fi
