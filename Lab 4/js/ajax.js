$(function(){
    $('#function1_form').ajaxForm({
        url:"/brew/f1.r",
        type:'POST',
        complete: function() {
            function1();
        }
    })
    $('#function2_form').ajaxForm({
        url:"/brew/f2.r",
        type:'POST',
        // dataType:JSON,
        complete: function() {
            function2();
        }
    })
    $('#function3_form').ajaxForm({
        url:"/brew/f3.r",
        type:'POST',
        complete: function() {
            function3();
        }
    })
});

function function1(data,textStatus,jqXHR){
    console.log(data);
}
function function2(data,textStatus,jqXHR){
    console.log(data);
}
function function3(data,textStatus,jqXHR){
    console.log(data);
}