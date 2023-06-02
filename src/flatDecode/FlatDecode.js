export function lookupVal(fn){
    return (key)=>{
        return fn[key]
    }
}

export const primitiveDecodeImpl = function (_type) {
    return (obj) => {
        return (success) => {
            return (failure) => {
                if (typeof obj === _type) {
                    return success(obj)
                } else {
                    return failure(`type mismatch: expected "${_type}", found "${typeof obj}"`)
                }
            }
        }
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
                    return failure(e.message)
                }
            }
        }
    }
}

export function maybeDecodeImpl(obj){
    return (failure)=>{
        return (nothing) => {
            return (decodeFn)=> {
                    if (obj === undefined || obj === null) {
                        return nothing
                    }
                    return decodeFn(obj)
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
                    return failure(e.message)
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
                return failure(e.message)
            }
        }
    }
}

export const mkDecodeEntry = function (k){
    return (fn)=>{
        // console.log(k)
        return [k, fn]
    }
}

function constructData(rows, obj, success, failure){
    try {
        const val = {}
        rows.forEach((x) => {
            val[x[0]] = x[1](obj[x[0]])((y) => y)((e) => {
                throw new Error(`while decoding ${x[0]} \n ${e}`)
            })
        })
        return success(val)
    }
    catch (e) {
        return failure(e.message)
    }
}

export function constructFromIterativeForm(rows){
    return (obj)=>{
        return (success)=>{
            return (failure)=>{
                return constructData(rows, obj, success, failure);
            }
        }
    }
}

export function constructNewTypeFromIterativeForm(rows){
    return (dataConstructor)=> {
        return (obj) => {
            return (success) => {
                return (failure) => {
                    return constructData(rows, obj, (x)=>success(dataConstructor(x)), failure)
                }
            }
        }
    }
}

export function arrayPush(arr){
    return (a)=>{
        try{
            // console.log(arr, a)
            arr.push(a)
            return arr
        }
        catch (e) {
            // console.log("is not array", e.toString())
            return arr;
        }
    }
}