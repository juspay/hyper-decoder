var timeTaken = 0;

export function stringify(fn){
    return JSON.stringify(fn)
}

var input1 = {
    "id": 1,
    "title": "Movie 1",
    "rating": 4.5,
    "year": 2022,
    "cast": ["Actor 1", "Actor 2", 3],
    "reviews": {
      "count": 10,
      "reviewers": [{name : "Reviewer 1", id: "asdf"}, {name : "Reviewer 2"},{name : "Reviewer 3"}]
    }
  };
  
  var input2 = {
    "id": null,
    "title": "Movie 2",
    "rating": 3.7,
    "year": 2019,
    "cast": ["Actor 4", "Actor 5"],
    "reviews": {
      "count": 5,
      "reviewers": null
    }
  };

export function movieData(){
    var x = []
    for(var i=0 ;i<2; i++){
        x.push(input1)
    }
    x.push(input2)
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

export function getTimeLine(){
    return { "wanted to sleep": "11 pm"
           , "completing last episode of day": "11:50 pm"
           , "started first episode of next day": "12:01 pm"}
}