var timeTaken = 0;

export function stringify(fn){
    return JSON.stringify(fn)
}

export function movieData(){
    var x = []
    for(var i=0 ;i<10; i++){
        x.push({cast : ["asdf", "123"], title : "afg", year: 2020, rating: 7.3, reviews : {count: 10}})
    }
    x.push({cast : ["asdf", "123"], title : "afg", year: 2023, rating: 7.3, reviews : {count: 10}})
    return x;
}

export function carData(){
    return {wheels : 5, fuelLevel: 10}
}

export function startProfile(){
    timeTaken = performance.now()
}

export function endProfile(){
    var val = performance.now() - timeTaken;
    console.log(val)
}