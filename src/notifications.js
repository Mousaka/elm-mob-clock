var elmDiv = document.getElementById('elm-app-lives-here');
var app = Elm.Main.embed(elmDiv);
const alarmPromise = document.getElementById('alarm');
var notificationsAllowed = false;

const notifcationsSupported = () => 'Notification' in window

const askForNotificationPermission = function(){
    if (!notifcationsSupported())  {
        notificationsAllowed = false;
    }else{
        window.Notification.requestPermission().then(function(result) {
            if (result === 'denied' || result === 'default') {
                notificationsAllowed = false;
            }else{
                notificationsAllowed = true;
            }
          });
    }
    return;
};

const showNotification = (nextUp) => {
    var n = new Notification('Next up', { 
        body: nextUp + ' is next'
        //icon: '/path/to/icon.png' // optional
    });
}

const soundTheAlarm = function(){
    alarmPromise.play().then(() => {
        // AUTOMATIC PLAYBACK
    }).catch(function(error) {
    // Automatic playback failed.
    console.log("Could not play", error);
    });
};

const setTitle = title => document.title = title

app.ports.setTitle.subscribe(setTitle)

app.ports.alarm.subscribe(() => {
  soundTheAlarm();
});

app.ports.notifyNext.subscribe((nextUp) => showNotification(nextUp));


askForNotificationPermission();