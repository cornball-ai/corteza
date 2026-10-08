# What the call loop's collaborators are in production.

# The SFU leg over a livekitr session (from livekitr::lk_connect, or
# mx.client::mx_call_join's `$session`). Audio is asked for at the
# detector's rate, mono, in short native frames; what the agent says
# goes out at whatever rate the synthesizer produced.
call_media_livekit <- function(session, rate = CALL_AUDIO_RATE) {
    if (!requireNamespace("livekitr", quietly = TRUE)) {
        stop("a call needs livekitr installed (it is a Suggests, since calls ",
             "are opt-in)", call. = FALSE)
    }
    list(identity = session$identity,
         on_audio = function(callback) {
        livekitr::lk_on_audio(session, callback, sample_rate = rate,
                              channels = 1L, frame_ms = CALL_VAD_FRAME_MS)
    },
         publish = function(samples, rate) {
        livekitr::lk_publish_pcm(session, as.integer(samples),
                                 sample_rate = as.integer(rate), channels = 1L,
                                 wait = TRUE)
    },
         poll = function(timeout) livekitr::lk_poll(session, timeout),
         disconnect = function() livekitr::lk_disconnect(session))
}

# The voice brain for one room: voice_turn() and voice_turn_report()
# over a voice state (voice_state()), with the room's turns recorded
# for reports. The 1:1 voice mode records turns per AgentVoice session;
# a call has one room and no sessions, so the room is the record.
call_brain <- function(state, room_id) {
    turns <- new.env(parent = emptyenv())
    list(turn = function(text, speaker, send) {
        voice_turn(state, room_id, text, send, turns, speaker = speaker)
    },
         report = function(turn_id, heard) {
        voice_turn_report(state, room_id, turns, turn_id, heard)
    },
         turns = turns)
}

# The speech routes as the loop's stt and tts functions.
call_speech <- function(media_cfg, http = .call_http) {
    list(stt = function(samples, rate) call_stt(media_cfg, samples, rate,
            http = http),
         tts = function(text) call_tts(media_cfg, text, http = http))
}
