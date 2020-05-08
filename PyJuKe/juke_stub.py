
def getrandkey(keyseed = '', keylen = 8):
    import uuid
    
    numkey = keylen - len(keyseed)
    if numkey > 0:
        key = str(uuid.uuid4())
        key = key[0:numkey]
        keyseed += key
    key = keyseed
    key = key[0:keylen]
    return key

