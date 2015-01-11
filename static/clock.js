var canvas = document.getElementById('cc');
var context = canvas.getContext('2d');

var mx = canvas.width / 2;
var my = canvas.height / 2;

var PI = 3.1415926535;

function drawClockBackground() {
    context.strokeText("12", 0, -120);
    context.strokeText("3", 120, 0);
    context.strokeText("6", 0, 120);
    context.strokeText("9", -120, 0);
}

function drawSecond(sec) {
    context.save();

    //context.clearRect(0, 0, canvas.width, canvas.height);
    context.translate(mx, my);

    drawClockBackground();

    var ang = 2 * PI / 60 * sec;
    context.rotate(ang);

    context.beginPath();
    context.moveTo(0, 0);
    context.lineTo(0, -100);
    context.strokeStyle = "green";
    context.stroke();


    context.restore();
}

function timerFunc() {
    var date = new Date();
    var hour = date.getHours();
    var min = date.getMinutes();
    var sec = date.getSeconds();

    context.clearRect(0, 0, canvas.width, canvas.height);

    var time_txt = hour + ' : ' + min + ' : ' + sec;
    var tw = context.measureText(time_txt).width;
    context.strokeText(time_txt, mx - tw / 2, 20);

    drawSecond(sec);
}

setInterval(timerFunc, 1000);


timerFunc();
