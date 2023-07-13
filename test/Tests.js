export function cmpr(a){
    return (b)=>{
        console.log(a);
        console.log(b);
        return a === b
    }
}