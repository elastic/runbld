exec > /dev/null 2>&1

if [[ "$BUILD_METADATA" =~ ^.*the.first.metadata.*$ &&
      "$BUILD_METADATA" =~ ^.*the.second.metadata.*$ ]]; then
    exit 0
else
    exit 1
fi
