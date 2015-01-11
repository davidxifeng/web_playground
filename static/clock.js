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

    // 保存画板 上下文
    context.save();

    // 移动画板原点到屏幕中间
    context.translate(mx, my);
    // 根据移动后的坐标, 画出数字背景
    drawClockBackground();

    // 根据当前秒钟数旋转画板
    context.rotate(2 * PI / 60 * sec);

    // 清空画图路径
    context.beginPath();

    // 画一条竖直向上 长度为100的线
    context.moveTo(0, 0);
    context.lineTo(0, -100);

    context.strokeStyle = "green";
    // 画线
    context.stroke();

    // 恢复画板默认状态
    context.restore();

    // 画分钟
    context.save();
    context.translate(mx, my);
    context.rotate(2 * PI / 12 * 5);
    context.beginPath();
    context.moveTo(0, 0);
    context.lineTo(0, -80);
    context.strokeStyle = "red";
    context.stroke();
    context.restore();

}

// 定时器回调函数
// 回调函数 callback function
function drawClock() {
    // 1. 获取当前时间
    var date = new Date();

    // 时分秒
    var hour = date.getHours();
    var min = date.getMinutes();
    var sec = date.getSeconds();

    // 2. 清空画板全部内容
    context.clearRect(0, 0, canvas.width, canvas.height);

    // 得到 如 21:54:21 这样的文字
    var time_txt = hour + ' : ' + min + ' : ' + sec;
    // 获取这段文字的宽度
    var tw = context.measureText(time_txt).width;
    // 计算坐标, 把这段文字画到屏幕中间偏上的位置
    context.strokeText(time_txt, mx - tw / 2, 20);

    // 画秒针 传入秒 参数
    drawSecond(sec);
}

// 设置定时器, 第一个参数是 时间到之后调用的函数,
// 第二个参数是定时器间隔 单位毫秒
setInterval(drawClock, 1000);
