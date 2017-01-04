#!/bin/bash

do_options() {
	while getopts ":pmrjs:t:" opt; do
		case $opt in
			p)
				for d in dev/dev*; do $d/bin/ssam ping; done
				;;
			m)
				dev/dev1/bin/ssam-admin member_status
				;;
			r)
				dev/dev1/bin/ssam-admin ring_status
				;;
			j)
				for d in dev/dev{2,3}; do $d/bin/ssam-admin join ssam1@127.0.0.1; done
				;;
			s)
				case "$OPTARG" in
					dev[1-9])
						dev/$OPTARG/bin/ssam start -pa apps/*/ebin
						dev/$OPTARG/bin/ssam attach
						;;
					all)
						#for d in dev/dev*; do $d/bin/ssam stop; done
						for d in dev/dev*; do $d/bin/ssam start; done
						;;
					*)
						show_usage
				esac
				;;
			t)
				case "$OPTARG" in
					dev[1-9])
						dev/$OPTARG/bin/ssam stop
						;;
					all)
						for d in dev/dev*; do $d/bin/ssam stop; done
						;;
					*)
						show_usage
						;;
				esac
				;;
			\?)
				show_usage
				;;
			:)
				echo ""
				echo "Option -$OPTARG requires arguments."
				show_usage
				exit
		esac
			
	done
}

show_usage() {
	echo ""
	echo "Usage:"
	echo "	-p : Sends ping to the node group"
	echo "	-m : Shows member status of the node group"
	echo "	-r : Shows ring status of the node group"
	echo "	-j : Joins the node group"
	echo "	-s dev[n] : Starts service on a node"
	echo "	   all    : Starts service on all node group"
	echo "	-t dev[n] : Terminates service on a node"
	echo "	   all    : Terminates service on all node group"
	echo ""
}

if [ $# == 0 ]
then
	show_usage
	exit
fi



do_options "$@"
