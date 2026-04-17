# Voice Chat Shiny App
# Push-to-talk voice interface for corteza

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #1a1a2e;
        color: #eee;
        font-family: system-ui, -apple-system, sans-serif;
      }
      .container-fluid { max-width: 800px; margin: 0 auto; padding: 20px; }
      h1 { color: #fff; text-align: center; margin-bottom: 30px; }

      #record-btn {
        width: 150px;
        height: 150px;
        border-radius: 50%;
        border: 4px solid #4a4a6a;
        background: linear-gradient(145deg, #2a2a4a, #1a1a2e);
        color: #fff;
        font-size: 18px;
        cursor: pointer;
        display: block;
        margin: 30px auto;
        transition: all 0.2s;
        box-shadow: 0 4px 15px rgba(0,0,0,0.3);
      }
      #record-btn:hover {
        border-color: #6a6a8a;
        transform: scale(1.05);
      }
      #record-btn.recording {
        background: linear-gradient(145deg, #8b0000, #5a0000);
        border-color: #ff4444;
        animation: pulse 1s infinite;
      }
      #record-btn.processing {
        background: linear-gradient(145deg, #4a4a0a, #3a3a00);
        border-color: #aaaa44;
      }

      @keyframes pulse {
        0%, 100% { box-shadow: 0 0 0 0 rgba(255,0,0,0.4); }
        50% { box-shadow: 0 0 0 20px rgba(255,0,0,0); }
      }

      #status {
        text-align: center;
        color: #888;
        margin: 20px 0;
        min-height: 24px;
      }

      #conversation {
        background: #0d0d1a;
        border-radius: 12px;
        padding: 20px;
        min-height: 300px;
        max-height: 500px;
        overflow-y: auto;
        margin-top: 20px;
      }

      .message {
        margin: 15px 0;
        padding: 12px 16px;
        border-radius: 12px;
        max-width: 85%;
      }
      .user-msg {
        background: #2d4a2d;
        margin-left: auto;
        text-align: right;
      }
      .assistant-msg {
        background: #2a2a4a;
        margin-right: auto;
      }
      .message-role {
        font-size: 11px;
        color: #888;
        margin-bottom: 4px;
      }

      #audio-player { display: none; }
    "))
  ),

  div(class = "container-fluid",
    h1("corteza Voice"),

    div(id = "status", "Click and hold to speak"),

    tags$button(
      id = "record-btn",
      onclick = "toggleRecording()",
      "Hold to Talk"
    ),

    div(id = "conversation"),

    tags$audio(id = "audio-player", controls = FALSE),

    # Hidden inputs for Shiny communication
    tags$input(type = "hidden", id = "audio-data", name = "audio-data"),

    tags$script(HTML("
      let mediaRecorder = null;
      let audioChunks = [];
      let isRecording = false;

      async function toggleRecording() {
        const btn = document.getElementById('record-btn');
        const status = document.getElementById('status');

        if (!isRecording) {
          // Start recording
          try {
            const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
            mediaRecorder = new MediaRecorder(stream, { mimeType: 'audio/webm' });
            audioChunks = [];

            mediaRecorder.ondataavailable = (e) => {
              audioChunks.push(e.data);
            };

            mediaRecorder.onstop = async () => {
              const audioBlob = new Blob(audioChunks, { type: 'audio/webm' });
              const reader = new FileReader();
              reader.onloadend = () => {
                const base64 = reader.result.split(',')[1];
                Shiny.setInputValue('audio_data', base64, {priority: 'event'});
              };
              reader.readAsDataURL(audioBlob);

              stream.getTracks().forEach(track => track.stop());
              status.textContent = 'Processing...';
              btn.classList.remove('recording');
              btn.classList.add('processing');
              btn.textContent = 'Processing';
            };

            mediaRecorder.start();
            isRecording = true;
            btn.classList.add('recording');
            btn.textContent = 'Recording...';
            status.textContent = 'Speak now, click again to stop';
          } catch (err) {
            status.textContent = 'Microphone access denied';
            console.error(err);
          }
        } else {
          // Stop recording
          if (mediaRecorder && mediaRecorder.state !== 'inactive') {
            mediaRecorder.stop();
          }
          isRecording = false;
        }
      }

      // Handle response audio
      Shiny.addCustomMessageHandler('playAudio', function(data) {
        const btn = document.getElementById('record-btn');
        const status = document.getElementById('status');

        if (data.audio) {
          const audio = document.getElementById('audio-player');
          audio.src = 'data:audio/wav;base64,' + data.audio;
          audio.play();
          audio.onended = () => {
            btn.classList.remove('processing');
            btn.textContent = 'Hold to Talk';
            status.textContent = 'Click and hold to speak';
          };
        } else {
          btn.classList.remove('processing');
          btn.textContent = 'Hold to Talk';
          status.textContent = 'Click and hold to speak';
        }
      });

      // Add message to conversation
      Shiny.addCustomMessageHandler('addMessage', function(data) {
        const conv = document.getElementById('conversation');
        const msgDiv = document.createElement('div');
        msgDiv.className = 'message ' + (data.role === 'user' ? 'user-msg' : 'assistant-msg');
        msgDiv.innerHTML = '<div class=\"message-role\">' + data.role + '</div>' + data.text;
        conv.appendChild(msgDiv);
        conv.scrollTop = conv.scrollHeight;
      });
    "))
  )
)

server <- function(input, output, session) {

  # Conversation history
  messages <- reactiveVal(list())

  # Load config
  config <- corteza:::load_config()

  observeEvent(input$audio_data, {
    req(input$audio_data)

    # Decode base64 audio
    audio_raw <- base64enc::base64decode(input$audio_data)

    # Save to temp file
    webm_file <- tempfile(fileext = ".webm")
    wav_file <- tempfile(fileext = ".wav")
    on.exit(unlink(c(webm_file, wav_file)), add = TRUE)

    writeBin(audio_raw, webm_file)

    # Convert to WAV using ffmpeg
    system2("ffmpeg", c(
      "-y", "-i", webm_file,
      "-ar", "16000", "-ac", "1",
      wav_file
    ), stdout = FALSE, stderr = FALSE)

    if (!file.exists(wav_file) || file.size(wav_file) < 1000) {
      session$sendCustomMessage("playAudio", list(audio = NULL))
      return()
    }

    # Transcribe with STT
    transcript <- tryCatch({
      stt_cfg <- config$voice$stt %||% list()
      port <- stt_cfg$port %||% 4123L
      stt.api::set_stt_base(sprintf("http://127.0.0.1:%d", port))
      stt.api::transcribe(wav_file)
    }, error = function(e) {
      message("STT error: ", e$message)
      NULL
    })

    if (is.null(transcript) || nchar(trimws(transcript)) == 0) {
      session$sendCustomMessage("playAudio", list(audio = NULL))
      return()
    }

    # Add user message to UI
    session$sendCustomMessage("addMessage", list(role = "user", text = transcript))

    # Build messages for LLM
    hist <- messages()
    hist <- c(hist, list(list(role = "user", content = transcript)))

    # Call LLM
    response <- tryCatch({
      llm.api::chat(hist, max_tokens = 500L)
    }, error = function(e) {
      message("LLM error: ", e$message)
      "Sorry, I couldn't process that."
    })

    # Update history
    hist <- c(hist, list(list(role = "assistant", content = response)))
    messages(hist)

    # Add assistant message to UI
    session$sendCustomMessage("addMessage", list(role = "assistant", text = response))

    # Generate TTS
    audio_base64 <- tryCatch({
      tts_cfg <- config$voice$tts %||% list()
      port <- tts_cfg$port %||% 7812L
      tts.api::set_tts_base(sprintf("http://127.0.0.1:%d", port))

      audio_file <- tempfile(fileext = ".wav")
      on.exit(unlink(audio_file), add = TRUE)

      tts.api::speak(response, file = audio_file)

      if (file.exists(audio_file)) {
        base64enc::base64encode(audio_file)
      } else {
        NULL
      }
    }, error = function(e) {
      message("TTS error: ", e$message)
      NULL
    })

    session$sendCustomMessage("playAudio", list(audio = audio_base64))
  })
}

shinyApp(ui, server)
