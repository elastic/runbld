exec > /dev/null 2>&1

if [ "$BUILD_METADATA" = "existing;metadata" ]; then
    exit 0;
else
    exit 1
fi
