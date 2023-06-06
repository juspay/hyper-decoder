var key_stack = []

function compare(a, b){
    if(typeof a !== typeof b){
        return {_: false, m:"type mismatch"}
    }

    if(Array.isArray(a) || Array.isArray(b)){
        if(! (Array.isArray(a) && Array.isArray(b))){
            return {_: false, m:"one one them is not array"}
        }
        if(a.length !== b.length){
            const temp = []
            try{
                b.forEach((x)=>{
                    if(!a.includes(x)){
                        temp.push(x)
                    }
                })
            }
            catch(e){}
            return {_: false, m:"array length not equal"}
        }
        for(var i=0; i<a.length; i++){
            const c = compare(a[i], b[i])
            if(!c._){
                return c
            }
        }
        return {_: true, m:""}
    }

    if(typeof a === "object"){
        const arr_check = compare(Object.keys(a).sort(), Object.keys(b).sort());
        const a_keys = Object.keys(a)
        if(arr_check._){
            for(var i=0; i<a_keys.length; i++){
                const k = a_keys[i]
                key_stack.push(k)
                const c = compare(a[k], b[k])
                if(! c._){
                    return c
                }
                key_stack.pop()
            }
            return {_: true, m:""}
        }
        return arr_check

    }

    const is_eq = a === b

    if (!is_eq){
        console.log("");
    }

    return {_: is_eq, m: key_stack}
}