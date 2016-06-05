#!/usr/bin/sh
#

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
	Rscript --vanilla src/paper_naive.r $DATA_DIR $OUT $SIZE
fi


