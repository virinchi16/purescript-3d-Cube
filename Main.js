"use strict";

exports.tim = function(ctx){
  return function(callb){
    function loop(val){
     callb(ctx)();
     //callb(ctx)(360.0)();
      window.requestAnimationFrame(loop);
    }

    window.requestAnimationFrame(loop);
    return function() {};
  }
}

exports.addEvents= function(ctx){
  ctx.rect(0, 0, 1000, 1000);
  ctx.fillStyle = "blue";
  return function(evt){
    return function(callb){
      function eventHandler(e){

        callb(e)();console.log(evt);//console.log(e);
      }
      one.addEventListener(evt,eventHandler);console.log(evt);
      return function(){};
    }
  }
}
