#usr/bin/env bash
#
# bash scripts to call differnte timedata program
#
_td ()   #  By convention, the function name
{        #+ starts with an underscore.
  local cur
  # Pointer to current completion word.
  # By convention, it's named "cur" but this isn't strictly necessary.

  COMPREPLY=()   # Array variable storing the possible completions.
  cur=${COMP_WORDS[COMP_CWORD]}

  case "$cur" in
    *)
    COMPREPLY=( $( compgen -W '--help axpy extend get magnitude norm multiply select ' $cur ) );;
#   Generate the completion matches and load them into $COMPREPLY array.
#   xx) May add more cases here.
#   yy)
#   zz)
  esac

  return 0
}

complete -F _td -o filenames ./td.sh
#        ^^ ^^^^^^^^^^^^  Invokes the function _UseGetOpt-2.

#complete -W "axpy get multiply select magnitude norm extend scale" td
# args=$@
echo 'hello '

# dir=`dirname "$0"`
# repo=`eval "cd $dir/../../;pwd;cd - > /dev/null"`

# command=$1
# args=@[2:]

# if [ "$command" == '--help' ]
# then
#     echo 'axpy'
#     echo 'get' 
#     echo 'multiply'
#     echo 'select' 
#     echo 'magnitude'
#     echo 'norm'
#     echo 'extend'
#     echo 'scale'
# elif [ "$command" =='axpy' ]:
# then
#     program=../axpy_timedata/axpy.out
# elif [ "$command" =='get' ]:
# then
#     program=../get_timedata/get_timedata.out
# elif [ "$command" =='multiply' ]:
# then
#     program=../multiply_timedata/multiply_timedata.out
# elif [ "$command" =='select' ]:
#     program=../select_timedata/select_timedata.out
# then
# elif [ "$command" =='magnitude']:
# then
#     program=../vector2magnitude_timedata/vector2magnitude_timedata.out
# elif [ "$command" =='norm']:
# then
#     program=../lp_norm_timedata/lp_norm_timedata.out
# elif [ "$command" =='extend']:
# then
#     program=../extend_timedata/extend_timedata.out
# elif [ "$command" =='scale']:
# then
#     program=../scale_timedata/scale_timedata.out
# fi
# $program $args


