/* queue suspend/resume protocol */

/* flags in the shared disk */
bool suspend /* suspend requested */
bool suspend_ack /* suspend acknowledged *.

/* the queue may have no data (none); a delta or a full sync.
   the full sync is performed immediately on resume. */
mtype = { sync delta none }
mtype inflight_data = none

proctype consumer(){

  /* get the channel back to a known state by suspending,
     resuming and receiving the initial resync */
resync:
  (suspend == suspend_ack)
  suspend = true;
  (suspend == suspend_ack)
resync2:
  /* drop old data */
  inflight_data = none;
  suspend = false;
  (suspend == suspend_ack)
  (inflight_data == sync)
  /* receive initial sync */
  inflight_data = none;
  do
  /* Consumer.pop */
  :: (inflight_data != none) ->
    /* In steady state we receive deltas */
    assert (suspend_ack == false);
    assert (inflight_data == delta);
    inflight_data = none
  /* Consumer.suspend */
  :: ((suspend == false)&&(suspend_ack == false)) ->
    goto resync
  /* Consumer.resume */
  :: ((suspend == true)&&(suspend_ack == true)) ->
    goto resync2
  od;
}

proctype producer(){
  do
  /* Producer.state = Running */
  :: ((suspend == false)&&(suspend_ack==true)) ->
    suspend_ack = false;
    inflight_data = sync
  /* Producer.state = Suspended */
  :: ((suspend == true) && (suspend_ack == false)) ->
    suspend_ack = true
  /* Producer.push */
  :: ((suspend == false) && (suspend_ack == false) && (inflight_data != sync)) ->
    inflight_data = delta
  od
}

init {
  atomic {
    run producer();
    run consumer();
  }
}
