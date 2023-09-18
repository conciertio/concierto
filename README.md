# Concierto

Concierto is a simple command line orchestration tool for docker/podman.

It's written in Clojure using the Babashka runtime so is fast to start.

Quick flavor ...

    ./concierto build all --push
    ./concierto deploy \
        --scenario cluster \
        --target stage \
        --select \ 
            cluster stage role db also \
            cluster stage role api also \
            cluster stage role balancer 

Find full documentation here https://www.conciert.io.

Status: Late alpha.
