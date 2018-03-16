
    var button1 = document.getElementById('Function1');
    var button2 = document.getElementById('Function2');
    var button3 = document.getElementById('Function3');
    document.getElementById('function2_box').style.display = 'none';
    document.getElementById('function3_box').style.display = 'none';
    button1.onclick = function(){box_display('function1_box','function2_box','function3_box');}
    button2.onclick = function(){box_display('function2_box','function1_box','function3_box');}
    button3.onclick = function(){box_display('function3_box','function2_box','function1_box');}
    
    
    function box_display(a,b,c){
        document.getElementById(b).style.display = 'none';
        document.getElementById(c).style.display = 'none';
        document.getElementById(a).style.display = 'block';
    }