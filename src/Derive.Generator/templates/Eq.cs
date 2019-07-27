partial class Eq : System.IEquatable<Eq>
{
    public string Field;
    public string Prop { get; }

    public bool Equals(Eq other)
        => System.Collections.Generic.EqualityComparer<string>.Default.Equals(Field, other.Field)
        && System.Collections.Generic.EqualityComparer<string>.Default.Equals(Prop, other.Prop);

    public override bool Equals(object obj) => !(obj is null) && obj is Eq other && Equals(other);

    public override int GetHashCode() => (Field, Prop).GetHashCode();

    public static bool operator ==(Eq left, Eq right) => left?.Equals(right) ?? right is null;
    public static bool operator !=(Eq left, Eq right) => !(left == right);

}