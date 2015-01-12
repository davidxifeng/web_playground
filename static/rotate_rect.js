var canvas = document.getElementById('cc');
var context = canvas.getContext('2d');

var mx = canvas.width / 2;
var my = canvas.height / 2;

var PI = 3.1415926535;

function drawRect(r, fs) {
    context.save();
    context.translate(mx, my);

    context.beginPath();

    context.moveTo(r.a.x, r.a.y);
    context.lineTo(r.b.x, r.b.y);

    context.moveTo(r.b.x, r.b.y);
    context.lineTo(r.c.x, r.c.y);

    context.moveTo(r.c.x, r.c.y);
    context.lineTo(r.d.x, r.d.y);

    context.moveTo(r.d.x, r.d.y);
    context.lineTo(r.a.x, r.a.y);

    context.strokeStyle = fs;
    context.stroke();

    context.restore();
}

// 旋转矩形 angle角度(角度) 返回旋转后的矩形的宽高
function rotate_rect(width, height, angle) {
    var rad = function(x) {
        var PI = 3.1415926535898
        return PI / 180 * x;
    }
    var hw = width / 2
    var hh = height / 2
    var len_tr = Math.sqrt(hw * hw + hh * hh);

    var na = Math.asin(hh / len_tr) + rad(angle)
    var nx = Math.cos(na) * len_tr
    var ny = Math.sin(na) * len_tr

    var ma = Math.acos(-hw / len_tr) + rad(angle)
    var mx = Math.cos(ma) * len_tr
    var my = Math.sin(ma) * len_tr

    var rx = Math.max(Math.abs(mx), Math.abs(nx))
    var ry = Math.max(Math.abs(my), Math.abs(ny))
    return {
        a : { x : nx, y : ny},
        b : { x : mx, y : my},
        c : { x : -nx, y : -ny},
        d : { x : -mx, y : -my},
    }
}

var r = rotate_rect(2, 2, 45);
console.log(r);

var rotate_ang = 0;
var fs_list = [
    'green',
    'red',
    'blue',
    'yellow',
    'purple',
];
var i = 0;
function updateRect() {
    //context.clearRect(0, 0, canvas.width, canvas.height);
    drawRect(rotate_rect(200, 200, rotate_ang), fs_list[i]);
    if ( rotate_ang % 90 == 0) {
        i = fs_list.length == i ? 0 : i + 1;
    }
    if (rotate_ang == 360) {
        rotate_ang = 0;
    } else {
        rotate_ang += 5;
    }
}

//for (var i=0; i < 18; ++i) {
    //updateRect();
//}

updateRect();
setInterval(updateRect, 100);
