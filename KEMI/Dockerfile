# Container image that runs your code
FROM swipl:stable

RUN apt-get update && apt-get install -y \
    git build-essential

# WORKDIR /code
# Copies your code file from your action repository to the filesystem path `/` of the container
COPY . /.

# Code file to execute when the docker container starts up (`entrypoint.sh`)
ENTRYPOINT ["/entrypoint.sh"]
