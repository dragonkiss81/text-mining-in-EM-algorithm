#!/usr/bin/sh

NUM=60

if [ $# -eq 0 ]
then
        echo "Missing options!"
        echo "(run $0 -h for help)"
        echo ""
        exit 0
fi

ECHO="false"

while getopts "i:o:n:" OPTION; do
        case $OPTION in
                i)
                        ECHO="true"
			DATA_DIR=$OPTARG
			echo "data-directory : $DATA_DIR"
                        ;;
                o)
                        ECHO="true"
			OUT=$OPTARG
			echo "outputfile : $OUT"
                        ;;
                n)
                        ECHO="true"
			SIZE=$OPTARG
			echo "labeled-data size : $SIZE"
                        ;;
             	?)
            		echo "unknown argument"
			exit 1
			;;

        esac
done

if [ $ECHO = "true" ]
then
	if [ "$SIZE" -lt "$NUM" ];
	then
		echo "type I EM"		
		Rscript --vanilla src/paper_EM.r $DATA_DIR $OUT $SIZE
	else
		echo "type II EM"
		Rscript --vanilla src/slide_EM.r $DATA_DIR $OUT $SIZE
	fi
fi


