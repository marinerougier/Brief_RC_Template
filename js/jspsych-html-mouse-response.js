/**
 * jspsych-serial-reaction-time
 * Josh de Leeuw
 *
 * plugin for running a serial reaction time task
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["html-mouse-response"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'html-mouse-response',
    description: '',
    parameters: {
      stimulus: {
        type: jsPsych.plugins.parameterType.HTML_STRING,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The HTML content to be displayed.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, the trial ends after a key press.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show the trial'
      },
    }
  }

  plugin.trial = function(display_element, trial) {

    display_element.innerHTML = '<div id="jspsych-html-mouse-response" class="jspsych-html-mouse-response">'+trial.stimulus+'</div>';

    var trial_data = {
      rt: null,
      picSelected: null,
    }

    var imgs = $('.jspsych-html-mouse-response img');

    for(var i=0; i<imgs.length; i++){
      imgs[i].addEventListener('mousedown', function(e){
        var info = {};
        info.img = e.currentTarget.getAttribute('src');
        info.rt = Date.now() - startTime;

        console.log(e.currentTarget.getAttribute('src'));

        after_response(info);
      });
    };

    startTime = Date.now();

    function endTrial(info) {
      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        "stimulus": trial.stimulus,
        "selectedImg": info.img,
        "rt": info.rt,
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);

    };

    // function to handle responses by the subject
    function after_response(info) {
      endTrial(info);
    };

  };

  return plugin;

})();
