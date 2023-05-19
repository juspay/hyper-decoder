export function decodeArr(fn){
    return function(fun){
        const ret = [];
        for (let i =0; i < fn.length ; i++) {
            ret.push(fun(fn[i]))
        }
        return ret;
    }
}

export function lookupVal(fn){
    return (key)=>{
        return fn[key]
    }
}

export function unsafeInsertImpl(key){
    return (val)=>{
        return (rec)=>{
            rec[key] = val
            return rec
        }
    }
}

export function decodeStrImpl(fn){
    if(typeof fn === "string"){
        return fn
    }
    throw new Error("")
}

export function decodeStrChain(obj){
    return (success)=>{
        return (failure)=>{
            try {
                if (typeof obj === "string") {
                    return success(obj);
                }
            }
            catch (e) {
                return failure("error while decoding string")
            }
            return failure("type is not string")
        }
    }
}

export function decodeIntImpl(fn){
    if(typeof fn === "number"){
        return fn
    }
    throw new Error("")
}

export function decodeNumImpl(fn){
    if(typeof fn === "number"){
        return fn
    }
    throw new Error("")
}

export function decodeBoolImpl(fn){
    if(typeof fn === "boolean"){
        return fn
    }
    throw new Error("")
}

export function decodeMaybeImpl(fn){
    return (fun)=>{
        return (nothing)=>{
            return (just)=>{
                if(fn === undefined){
                    return nothing
                }
                return just(fun(fn))
            }
        }
    }
}

export function safeDecodeImpl(fn){
    return (err)=>{
        return (val)=>{
            return (fun)=>{
                try{
                    return val(fun(fn));
                } catch (e) {
                    return err(e.toString())
                }
            }
        }
    }
}

export function throwErr(err){
    throw new Error(err)
}