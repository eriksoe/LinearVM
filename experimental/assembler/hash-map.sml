signature HASH_MAP = sig
    type (''k,'v) HashMap;
    val create : (''k -> word) -> int -> (''k,'v) HashMap;
    val bucket_count : (''k,'v) HashMap -> int;
    val insert : (''k,'v) HashMap -> ''k * 'v -> unit;
    val contains : (''k,'v) HashMap -> ''k -> bool;
    val find : (''k,'v) HashMap -> ''k -> 'v option;
    val remove : (''k,'v) HashMap -> ''k -> unit;
end;


structure HashMap : HASH_MAP = struct
    type (''k,'v) HashMap = {buckets: (''k * 'v) list array, hashfun: ''k -> word};

    fun bucket_count ({buckets,...}:(''k,'v) HashMap) = Array.length buckets

    fun bucket_for_key (hm as {hashfun,...} :(''k,'v) HashMap) key =
        Word.toInt(Word.mod(hashfun(key), Word.fromInt(bucket_count hm)))

    fun get_bucket ({buckets,...}:(''k,'v) HashMap, b) =
        Array.sub(buckets, b)

    fun transform_bucket ({buckets,...}:(''k,'v) HashMap, b, f) =
        let val old = Array.sub(buckets, b);
            val new = f old
        in Array.update(buckets, b, new)
        end

  fun create hashfun size : (''k,'v) HashMap =
      {hashfun=hashfun,
       buckets=Array.tabulate(size, fn(_)=>[])}

  fun insert (hm as {hashfun,buckets}) (k,v) =
      let val b = bucket_for_key hm k
      in transform_bucket(hm, b, fn kvs => (k,v)::kvs)
      end

  fun contains (hm as {hashfun,buckets}) k : bool =
      List.exists (fn(k',v) => k'=k) (get_bucket(hm, bucket_for_key hm k))

  fun find (hm as {hashfun,buckets}) k =
      let fun aux [] = NONE
            | aux((k',v)::kvs) = if k'=k then SOME v
                                 else aux kvs
      in aux (get_bucket(hm, bucket_for_key hm k))
      end

  fun remove (hm as {hashfun,buckets}) k =
      let val b = bucket_for_key hm k
          fun aux [] = []
            | aux ((kv as (k',v))::kvs) = if k'=k then kvs
                                          else kv::(aux kvs)
      in transform_bucket(hm, b, aux)
      end
end
