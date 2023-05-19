export function stringDecodeImpl(obj, success, failure){
    return primitiveDecodeImpl(obj, "string", success, failure)
}

export function intDecodeImpl(obj){
    return (success)=>{
        return (failure)=>{
            return primitiveDecodeImpl(obj, "number", success, failure)
        }
    }
}

export function numDecodeImpl(obj){
    return (success)=>{
        return (failure)=>{
            return primitiveDecodeImpl(obj, "number", success, failure)
        }
    }
}

export function bitDecodeImpl(obj){
    return (success)=>{
        return (failure)=>{
            return primitiveDecodeImpl(obj, "boolean", success, failure)
        }
    }
}

function primitiveDecodeImpl(obj, _type, success, failure){
    try{
        if(typeof obj === _type){
            return success(obj)
        } else{
            return failure("type is not " + _type)
        }
    } catch (e) {
        return failure(e.toString())
    }
}

export function arrDecodeImpl(obj){
    return (decodeFn)=>{
        return (success)=>{
            return (failure)=>{
                try {
                    const arrToRet = [];
                    if(Array.isArray(obj)){
                        obj.forEach(curr=>arrToRet.push(decodeFn(curr)))
                        return success(arrToRet);
                    } else{
                        return failure("type is not array")
                    }
                } catch (e) {
                    return failure(e.toString())
                }
            }
        }
    }
}

export function maybeDecodeImpl(obj){
        return (failure)=>{
            return (nothing) => {
                    return (decodeFn)=> {
                        try {
                            if (typeof obj === 'undefined') {
                                return nothing
                            }
                            return decodeFn(obj)

                        } catch (e) {
                            return failure(e.toString())
                        }
            }
        }
    }
}

export function tryCatch(obj){
    return (recordDecodeFn)=>{
        return (success)=>{
            return (failure)=>{
                try{
                    return success(recordDecodeFn(obj))
                }
                catch (e) {
                    return failure(e.toString())
                }
            }
        }
    }
}

export function shortCircuit(err){
    throw new Error(err)
}

export function tryWithString(str){
    return (decodeFn)=>{
        return (failure)=>{
            try{
                return decodeFn(JSON.parse(str))
            } catch (e) {
                return failure(e.toString())
            }
        }
    }
}

export function storeSomewhere(a){
    // if(!window.tailTrace){
    //     window.tailTrace = []
    // }
    // window.tailTrace.push(a)
    return a
}