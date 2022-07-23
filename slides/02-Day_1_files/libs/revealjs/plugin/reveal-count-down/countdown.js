const RevealCountDown = {
  id: 'RevealCountDown',
  init: ( deck ) => {
    var options = deck.getConfig().countdown || {};
    
    var defaultOptions = {
      defaultTime: 300,
      autostart: "no",
      tDelta: 30
    };

    defaults(options, defaultOptions);

    function defaults(options, defaultOptions) {
      for (var i in defaultOptions) {
        if (!options.hasOwnProperty(i)) {
          options[i] = defaultOptions[i];
        }
      }
    }

    var counterRef = null;
    var interval = null;
    var startTime = 0;
    var elapsedTime = 0;
    var running = false;

    deck.addEventListener("slidechanged", function(event) {
      initCountDown(event.currentSlide);
    });

    // If timer is on first slide start on ready
    deck.addEventListener("ready", function(event) {
      initCountDown(event.currentSlide);
    });

    deck.addKeyBinding(
      { keyCode: 84, key: "T", description: "Pause/Unpause timer" },
      function() {
        togglePauseTimer();
      }
    );

    deck.addKeyBinding(
      {
        keyCode: 187,
        key: "+",
        description: "Increase timer with tDelta seconds"
      },
      increaseTime
    );

    deck.addKeyBinding(
      {
        keyCode: 189,
        key: "-",
        description: "Decrease time with tDelta seconds"
      },
      decreseTime
    );

    function updateTimer(timeLeft) {
      if (counterRef === null) return;
      secondsLeft = timeLeft;
      minutesLeft = Math.floor(secondsLeft / 60);
      secondsLeft = secondsLeft % 60;
      hoursLeft = Math.floor(minutesLeft / 60);
      minutesLeft = minutesLeft % 60;

      if (hoursLeft == 0 && minutesLeft <= 0) {
        if (secondsLeft <=20) {
          counterRef.style.color = "#CB444A";
          counterRef.style.borderColor = "#CB444A";
        } else {
          counterRef.style.color = "#F6C344";
          counterRef.style.borderColor = "#F6C344";
        }
      }
      
      if (hoursLeft > 0) {
        counterRef.innerHTML =
          hoursLeft + "h " + minutesLeft + "m " + secondsLeft + "s";
      } else if (minutesLeft > 0) {
        counterRef.innerHTML = minutesLeft + "m " + secondsLeft + "s";
      } else if (minutesLeft <= 0 && secondsLeft > 0) {
        counterRef.innerHTML = secondsLeft + "s";
      } else {
        counterRef.innerHTML = "Time is up";
      }
    }

    function increaseTime() {
      startTime = Number(startTime) + Number(options.tDelta);
      updateTimer(startTime - elapsedTime);
    }

    function decreseTime() {
      startTime = Number(startTime) - Number(options.tDelta);
      if (startTime < elapsedTime) startTime = elapsedTime;
      updateTimer(startTime - elapsedTime);
    }

    function togglePauseTimer() {
      running = !running;
    }

    function startTimer() {
      interval = setInterval(async function() {
        if (elapsedTime < startTime && running && !Reveal.isPaused()) {
          elapsedTime = elapsedTime + 1;
          updateTimer(startTime - elapsedTime);
        }
      }, 1000);
    }

    function initCountDown(currentSlide) {
      if (interval != null) clearInterval(interval);
      counterRef = currentSlide.getElementsByTagName("countdown")[0];
      if (counterRef === undefined) return;
      time = counterRef.getAttribute("time");
      autostart = counterRef.getAttribute("autostart");
      elapsedTime = 0;
      startTime = time ? time : options.defaultTime;
      
      counterRef.addEventListener("click", function(event){
        togglePauseTimer();
      })
      
      startTimer();
      updateTimer(startTime - elapsedTime);
      running = autostart === "yes" ? true : false;
    }
  }
};
