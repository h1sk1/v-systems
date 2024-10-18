{ pkgs, lib, config, inputs, ... }:

{
    languages.scala.enable = true;
    languages.scala.sbt.enable = true;

    packages = [ pkgs.curl pkgs.git pkgs.jdk21 pkgs.scala pkgs.sbt ];

    env = {
        VSYS_JAR_FILE = "./target/vsys-all-0.4.2.jar";
        VSYS_CONF_FILE = "./vsys-mainnet.conf";
    };

    processes = {
        build.exec = ''
        if [ ! -e "$VSYS_JAR_FILE" ]; then
            sbt packageAll
            else echo "Jar file already exists, skipping build...";
        fi'';

        run.exec = ''
        echo "Waiting for jar file to be built...";
        while [ ! -e "$VSYS_JAR_FILE" ]; do
            sleep 1;
        done;
        sleep 5;

        echo "Starting VSYS node...";
        for attempt in seq 1 5; do
            java -jar $VSYS_JAR_FILE $VSYS_CONF_FILE;
            if [ $? -eq 0 ]; then
                break;
            fi;
            
            if [ $attempt -eq 1 ]; then
                echo "Failed to start VSYS node, retrying...";
            fi;

            if [ $attempt -eq 5 ]; then
                echo "Failed to start VSYS node, exiting...";
                exit 1;
            fi;

            sleep 5;
        done'';
    };

    enterShell = ''
    echo "SBT environment"
    '';
}
