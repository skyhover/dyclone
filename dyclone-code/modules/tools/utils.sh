###############################################################
#
# define a set of functions commonly used by other scripts
#
###############################################################


#111111111111111111111111111111111111111111111111111111
# functions for dealing with the EXT3_LINK_MAX limit
#

DEFAULTPREFIX=DIRRTT
DEFAULTMAXFILELIMIT=5000
DEFAULTMAXLINKLIMIT=5000
# the sum is better to <= 32000

# reserve $prefix.0.0 to store things we don't want to be reinfraplaced.

# Create a subdirectory ($prefix.level.id) and return its name (relative)
# parameters:
# $1 - the root path
# $2 - the prefix (default DIRRTT)
# $3 - the dir limit (default DEFAULTMAXLINKLIMIT)
preinfraplacement () {
    if ! [ -d $1 ]; then
        echo "Error: directory $1 invalid" 1>&2
        return 1
    fi
    local prefix=$DEFAULTPREFIX
    if [ "$2" != "" ]; then
        prefix=$2
    fi
    local dlimit=$DEFAULTMAXLINKLIMIT
    if [ "$3" != "" ]; then
        dlimit=$3
    fi

    local maxid=0
    local id=0
    local maxlevel=1
    local level=1
    local fn=   # use this to avoid name-conflicting bug!
    local tmpfn=
    while read fn; do
        id=${fn##*.}
        if [ $id -gt $maxid ]; then
            maxid=$id
        fi
        tmpfn=${fn%.*}
        level=${tmpfn##*.}
        if [ $level -gt $maxlevel ]; then
            maxlevel=$level
        fi
    done < <( find $1 -maxdepth 1 -name "$prefix.*" )
    maxid=$((maxid+1))
    if [ $maxid -ge $dlimit ]; then
        maxlevel=$((maxlevel+1))
        maxid=1
    fi
    
    mkdir -p $1/$prefix.$maxlevel.$maxid
    echo "$prefix.$maxlevel.$maxid"
}


# Create a subdirectory ($prefix.level.id) and move the files/directories with certain prefix into it.
# parameters:
# $1 - the root path
# $2 - the prefix (default DIRRTT)
# return 0 only if succeeded somehow; "level", "id" start from 1
infraplacement () {
    if ! [ -d $1 ]; then
        echo "Error: directory $1 invalid" 1>&2
        return 1
    fi
    local prefix=$DEFAULTPREFIX
    if [ "$2" != "" ]; then
        prefix=$2
    fi

    local maxid=0
    local id=0
    local maxlevel=1
    local level=1
    local fn=   # use this to avoid name-conflicting bug!
    local tmpfn=
    while read fn; do
        id=${fn##*.}
        if [ $id -gt $maxid ]; then
            maxid=$id
        fi
        tmpfn=${fn%.*}
        level=${tmpfn##*.}
        if [ $level -gt $maxlevel ]; then
            maxlevel=$level
        fi
    done < <( find $1 -maxdepth 1 -name "$prefix.*" )
    maxid=$((maxid+1))

    local newdir=$1/$prefix.$maxlevel.$maxid
    mkdir $newdir && (
    while read fn; do
        if [[ "$fn" != "$1" ]]; then
            mv $fn $newdir
        fi
    done < <( find $1 -maxdepth 1 -not -name "$prefix.*" ) )
    return $?
}

# Create a subdirectory ($prefix.level.id) and move all other files/directories into it.
# parameters:
# $1 - the root path
# $2 - the prefix (default DIRRTT)
# return 0 only if succeeded somehow; "level", "id" start from 1
infraplacement2 () {
    if ! [ -d $1 ]; then
        echo "Error: directory $1 invalid" 1>&2
        return 1
    fi
    local prefix=$DEFAULTPREFIX
    if [ "$2" != "" ]; then
        prefix=$2
    fi

    local maxlevel=0
    local level=0
    local fn=   # use this to avoid name-conflicting bug!
    local tmpfn=
    while read fn; do
        tmpfn=${fn%.*}
        level=${tmpfn##*.}
        if [ $level -gt $maxlevel ]; then
            maxlevel=$level
        fi
    done < <( find $1 -maxdepth 1 -name "$prefix.*" )
    maxlevel=$((maxlevel+1))

    local newdir=$1/$prefix.$maxlevel.1
    mkdir $newdir && (
    while read fn; do
        if [[ "$fn" != "$1" ]]; then
            mv $fn $newdir
        fi
    done < <( find $1 -maxdepth 1 -not -name "$prefix.$maxlevel.1" ) )
    return $?
}

# check a directory, if it is larger than the limit, create subdirectories to make it fit.
# parameters:
# $1 - the root path
# $2 - the prefix (default DEFAULTPREFIX)
# $3 - the file limit (default DEFAULTMAXFILELIMIT)
# $4 - the dir limit (default DEFAULTMAXLINKLIMIT)
# return 0 only if succeeded somehow; "level", "id" start from 1
reinfraplacement () {
    if ! [ -d $1 ]; then
        echo "Error: directory $1 invalid" 1>&2
        return 1
    fi
    local prefix=$DEFAULTPREFIX
    if [ "$2" != "" ]; then
        prefix=$2
    fi
    local flimit=$DEFAULTMAXFILELIMIT
    if [ "$3" != "" ]; then
        flimit=$3
    fi
    local dlimit=$DEFAULTMAXLINKLIMIT
    if [ "$4" != "" ]; then
        dlimit=$4
    fi

    local fnumber=`find $1 -maxdepth 1 -not -name "$prefix.*" | wc -l`
    if [ $fnumber -ge $flimit ]; then
        infraplacement $1 $prefix
        if [ $? -ne 0 ]; then
            echo "Failure in reinfraplacement: infraplacement $1 $prefix" 1>&2
            return 1
        fi
    fi
    local dnumber=`find $1 -maxdepth 1 -name "$prefix.*" | wc -l`
    if [ $dnumber -ge $dlimit ]; then
        infraplacement2 $1 $prefix
        if [ $? -ne 0 ]; then
            echo "Failure in reinfraplacement: infraplacement2 $1 $prefix" 1>&2
            return 1
        fi
    fi
    return 0
}


# Move a file/directory into a directory, possibly with "infraplacement"
# $1 - the file/dir to be moved
# $2 - the destination
# $3 - the prefix (default DIRRTT)
# $4 - the file limit (default DEFAULTMAXFILELIMIT)
# return 0 only if $1 is moved into $2 
moveinto () {
    if ! [ -e $1 ]; then
        echo "Error: source $1 not exist" 1>&2
        return 1
    fi
    if ! [ -d $2 ]; then
        echo "Error: directory $2 invalid" 1>&2
        return 1
    fi
    local prefix=$DEFAULTPREFIX
    if [ "$3" != "" ]; then
        prefix=$3
    fi
    local flimit=$DEFAULTMAXFILELIMIT
    if [ "$4" != "" ]; then
        flimit=$4
    fi

    reinfraplacement $2 $prefix $flimit 
    if [ $? -ne 0 ]; then
        echo "Failure: reinfraplacement $2 $prefix $flimit" 1>&2
        return 1
    fi

    mv $1 $2
    return $?
}


#2222222222222222222222222222222222222222222222
# functions for handling cluster jobs
#

# global (for each process calling this script) counter for the times we sleep
sleepcounter=0
# Hold on submitting a new job for a while
# $1 - cluster node limit
# $2 - sleep time (default 20s)
# $3 - the max number of times to hold on (default unlimited)
# NOTE: to hold on submission within a job may cause subtle deadlocks: the submitting jobs are still occupying one slot in the cluster queue while new submitted jobs  also want to submit another round of new jobs...
# Add '$3' to help leviate the problem; still require some manual intervention though...TODO
holdonSubmitJobs () {
    local clusternodenumber=$1
    local sleeptime=20
    local jobsinqueue=0
    local maxtime=0

    if [ "$2" != "" ]; then
        sleeptime=$2
    fi
    if [ "$3" != "" ]; then
        maxtime=$3
    fi
    while true; do
        jobsinqueue=$(expr `qstat | wc -l` - 2)
        if [ $jobsinqueue -gt $clusternodenumber ]; then
            if [[ $maxtime -gt 0 && $sleepcounter -ge $maxtime ]]; then
                return 1
            else
                sleepcounter=$((sleepcounter+1))
                echo "hold on jobs (limit $clusternodenumber): $sleeptime for $sleepcounter times..."
                sleep $sleeptime
                continue
            fi
        else
            return 0
        fi
    done
}

# wait for all jobs to finish
# $1 - the number of jobs
# $2 - the name of the array of scriptjobs
#      each element should be $jobname.$jobid
#      $jobname should be in the form of *.$jobnumber.sh
# $3 - sleep time (default 20s)
waitforalljobs () {
    local jobnumber=$1
    local sleeptime=20
    if [ "$3" != "" ]; then
        sleeptime=$3
    fi

    local scriptjobnumber=$(eval echo \${#$2[@]})
    if [ $jobnumber -ne $scriptjobnumber ]; then
        echo "Warning: $1 is not the same size as length($2). We do things for '$2' anyway."
    fi
    if [ $jobnumber -gt $scriptjobnumber ]; then
        jobnumber=$scriptjobnumber
    fi

    if [ $scriptjobnumber -le 0 ]; then
        return 0
    fi

    eval "local jobsLeft=( \${$2[@]} )"
    while true; do
        local job=
        local leftJobs=( )
        for job in ${jobsLeft[@]}; do
            if ! [ -f "${job%.*}.done" ]; then
                leftJobs=( ${leftJobs[@]} $job )
            else
                echo "Job ${job##*.} Done: $job"
                # may leave the done files there for inspection...
                # rm -f ${job%.*}.done
                # rm -f ${job%.*}.o* ${job%.*}.e*
                # we could always delete them later (if space is not a problem...)
            fi
        done
        local jn=${#leftJobs[@]}
        if [ $jn -le 0 ]; then
            return 0
        fi
        jobsLeft=( ${leftJobs[@]} )
        echo "still waiting for $jn jobs. Sleep $sleeptime seconds ..."
        sleep $sleeptime
    done
}

# wait a period before killing a proc; return when pid invalid or killed once
# $1 - pid to kill
# $2 - max wait time (default 5s)
# $3 - sleep time (default half of the wait time)
# NOTE: one jiffy is about 10 millisecond (0.01s); $14+$15 in /proc/pid/stat
waittokillpid ()
{
    local waittime=4
    if [ "$2" != "" ]; then
        waittime=$2
    fi
    local sleeptime=`expr $waittime / 2`
    if [ "$3" != "" ]; then
        sleeptime=$3
    fi
    local pidtokill=$1
    local elapsedtime=0
    local readtokill=0

    while true; do
        #kill -0 $pidtokill
        #if [ $? -ne 0 ]; then  # the pid no longer exists
        #    echo "pid not exist: ($pidtokill) at time `date` on $HOSTNAME" 1>&2
        #    return 0
        #fi
        # the above may still have race conditions,
        # so, changed to use the following:
        elapsedtime=`head -n 1 /proc/$pidtokill/stat 2> /dev/null`
        if [ $? -ne 0 ]; then  # the pid no longer exists
            echo "pid not exist: ($pidtokill) at time `date` on $HOSTNAME" 1>&2
            return 0
        fi
        elapsedtime=`echo $elapsedtime | awk '{print $14+$15}'`  # this is in milliseconds

        if [[ $waittime -gt 0 && $waittime -lt 1000 ]]; then  # assume seconds if "too small"
            if [ $elapsedtime -ge `expr $waittime \* 100` ]; then
                readtokill=1
            fi
        else  # assume microseconds
            if [ `expr $elapsedtime \* 10000` -ge $waittime ]; then
                readtokill=1
            fi
        fi
        if [ $readtokill -eq 1 ]; then
            kill -9 $pidtokill
            echo "Killed pid: ($pidtokill) at time `date` on $HOSTNAME" 1>&2
            return 0
        fi

        if [[ $waittime -gt 0 && $waittime -lt 1000 ]]; then  # assume seconds if "too small"
            sleep $sleeptime
        else
            usleep $sleeptime
        fi
    done
}

#333333333333333333333333333333333333333333333333
# functions for handling file names
#

# check whether a path is absolute or not
# $1 - the path name; don't have to exist
# return 0 for true. Should work for any Bourne-type shell
isAbsPath () {
  case $1 in
    /*) return 0;;
    *) return 1;;
  esac
}

# return the full path name of the output file for the code trunk
# $1 - the path name (may be a link) to the trunk
# return 0 if successful
codetrunkname () {
    local codefile=`readlink -e -n "$1"`
    if [ "$codefile" == "" ]; then
        echo "codetrunkname Warning: '$1' is not a valid symbolic link" 1>&2
        return 1
    fi
    local trn=${codefile%.*}
    if [ "$trn" == "" ]; then
        echo "codetrunkname Warning: '$1' is not a valid code trunk name" 1>&2
        return 1
    fi
    echo "$trn"
    return 0
}

# get the obj file name of the header;
# parameter:
# $1 - a code file (either the link or the actual file)
# These are dependent on the naming convention of our code chopper.
# Update: change .o to .so since we are using shared libraries to save disk spaces
headerObjFile () {
    local codefile=`readlink -e -n "$1"`  # $codefile should be the abs path to the code itself
    local headerfile=$(head -n 2 $codefile | tail -n 1 | cut -d'"' -f2)
    local headerbase=`basename $headerfile`
    local headerobjbase=${headerbase%.c.h}.so   # the .o file should exist before hand
    local headerdir=`dirname $headerfile`
    if ( isAbsPath $headerdir ); then
      headerfile=$headerdir/$headerobjbase
      if [ -f "$headerfile" ]; then
        echo "$headerfile"
        return 0
      else
        echo "Error: can't find the header object file for $1" 1>&2
        return 1
      fi
    else
      headerdir=`dirname $codefile`/$headerdir
      headerfile=$headerdir/$headerobjbase
      # search for the obj from the specified dir upward upto two levels
      local i=0
      while [ $i -le 2 ]; do
        if [ -f "$headerfile" ]; then  # -f test can follow symlinks
          if [ $i -gt 0 ]; then
            # add links to the headers, so that gcc can compile
            ( cd $headerdir &&
              ln -s `dirname ${headerfile#$headerdir/}`/$headerbase $headerbase
              ln -s ${headerfile#$headerdir/} $headerobjbase
            )
          fi
          headerfile=$(cd `dirname $headerfile`; pwd)/$headerobjbase
          echo "$headerfile"
          return 0
        else
          headerfile=`dirname $headerfile`/../$headerobjbase
        fi
        i=$((i+1))
      done
      echo "Error: can't find the header object file for $1" 1>&2
      return 1
    fi
}


# remove a directory which may contain a lot of files, and create a new one with the same name
# $1 - the dir to be removed
reflashdir ()
{
    if [ "$1" == "" ]; then
        echo "Error: '$1' invalid dir name" 1>&2
        return 1
    fi
    if [ -d $1 ]; then
        rm -rf $1
    elif [ -e $1 ]; then
        echo "Error: '$1' not a directory" 1>&2
        return 1
    fi
    mkdir -p $1
    if [ -d $1 ]; then
        return 0
    else
        return 1
    fi
} 

