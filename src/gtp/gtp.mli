type gtp_status = Ok | Error | Quit;;
type cmd_response = {status: gtp_status; result: string};;
type cmd_record = {name: string; ftn: string->cmd_response};;
val gtp_main : cmd_record list -> unit
