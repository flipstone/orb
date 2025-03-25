if [ "$IN_DEV_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  docker compose build
  exec docker compose run --rm dev $0 "$@"
fi
