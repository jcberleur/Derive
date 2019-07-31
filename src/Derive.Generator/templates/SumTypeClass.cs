[Newtonsoft.Json.JsonConverter(typeof(SumTypeClass.JsonConverter))]
abstract partial class SumTypeClass
{
    private SumTypeClass() { }

    public enum Discriminant
    {
        A
    }

    public abstract Discriminant Case { get; }

    public T Match<T>(System.Func<A, T> a)
    {
        switch (this)
        {
            case A self: return a(self);
            default:
                throw new System.ArgumentOutOfRangeException(nameof(Case), this, @"Unreachable");
        }
    }

    public void Switch(System.Action<A> a)
    {
        switch (this)
        {
            case A self: a(self); return;
            default:
                throw new System.ArgumentOutOfRangeException(nameof(Case), this, @"Unreachable");
        }
    }

    [Newtonsoft.Json.JsonConverter(typeof(SumTypeClass.DefaultConverter))]
    public sealed partial class A : SumTypeClass
    {
        public override Discriminant Case => Discriminant.A;
    }

    public class JsonConverter : Newtonsoft.Json.JsonConverter
    {
        private readonly System.Collections.Generic.Dictionary<Discriminant, System.Type> _cases = new System.Collections.Generic.Dictionary<Discriminant, System.Type>
        {
            {Discriminant.A, typeof(A)}
        };

        public override bool CanWrite => false;

        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object value, Newtonsoft.Json.JsonSerializer serializer)
        {
            throw new System.NotSupportedException("Unnecessary because CanWrite is false. The type will skip the converter.");
        }

        public override object ReadJson(Newtonsoft.Json.JsonReader reader, Type objectType, object existingValue, Newtonsoft.Json.JsonSerializer serializer)
        {
            var jObject = serializer.Deserialize<Newtonsoft.Json.Linq.JObject>(reader);
            if (!jObject.TryGetValue(nameof(SumTypeClass.Case), out var discriminantToken))
            {
                throw new Newtonsoft.Json.JsonSerializationException($"Missing required property {nameof(SumTypeClass.Case)} on Type {typeof(SumTypeClass)}");
            }
            var discriminant = discriminantToken.ToObject<Discriminant>(serializer);
            return jObject.ToObject(_cases[discriminant], serializer);
        }

        public override bool CanConvert(System.Type objectType) => typeof(SumTypeClass).IsAssignableFrom(objectType);
    }

    public class ResultJsonConverter : Newtonsoft.Json.JsonConverter
    {
        private readonly System.Collections.Concurrent.ConcurrentDictionary<(Type, Type), JsonConverter> _cache = new System.Collections.Concurrent.ConcurrentDictionary<(Type, Type), JsonConverter>();

        public override bool CanWrite => false;

        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object value, Newtonsoft.Json.JsonSerializer serializer)
        {
            throw new System.NotSupportedException("Unnecessary because CanWrite is false. The type will skip the converter.");
        }

        public override object ReadJson(Newtonsoft.Json.JsonReader reader, Type objectType, object existingValue, Newtonsoft.Json.JsonSerializer serializer)
        {
            return ResolveJsonConverter(objectType).ReadJson(reader, objectType, existingValue, serializer);
        }

        private JsonConverter ResolveJsonConverter(Type objectType)
        {
            var arguments = objectType.GetTypeInfo().GenericTypeArguments;
            return _cache.GetOrAdd(
                (arguments[0], arguments[1]),
                tuple => (JsonConverter)Activator.CreateInstance(typeof(ResultJsonConverter<,>).MakeGenericType(tuple.Item1, tuple.Item2)));
        }

        public override bool CanConvert(System.Type objectType) => objectType.IsGenericType && objectType.GetGenericTypeDefinition() == typeof(Result<,>);
    }

    public class DefaultConverter : Newtonsoft.Json.JsonConverter
    {
        public override bool CanWrite => false;

        public override void WriteJson(Newtonsoft.Json.JsonWriter writer, object value, Newtonsoft.Json.JsonSerializer serializer)
        {
            throw new System.NotSupportedException("Unnecessary because CanWrite is false. The type will skip the converter.");
        }

        public override bool CanRead => false;

        public override object ReadJson(Newtonsoft.Json.JsonReader reader, Type objectType, object existingValue, Newtonsoft.Json.JsonSerializer serializer)
        {
            throw new System.NotSupportedException("Unnecessary because CanRead is false. The type will skip the converter.");
        }

        public override bool CanConvert(System.Type objectType) => true;
    }
}