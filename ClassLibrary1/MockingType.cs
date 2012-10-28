using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    public enum HalfType
    {
        SingleToSingle,
        SetToSingle,
        SetToSet,
        AnyToAny
    }
    public struct MbType
    {
        private HalfType g, s;
        public HalfType G { get { return g;}}
        public HalfType S { get { return s;}}

        public MbType(HalfType G, HalfType S)
        {
            g = G;
            s = S;
        }

        private static string GetName(HalfType t)
        {
            switch (t)
            {
                case HalfType.SingleToSingle: return "1->1";
                case HalfType.SetToSingle: return "[]->1";
                case HalfType.SetToSet: return "[]->[]";
                case HalfType.AnyToAny: return "a->a";
            }
            throw new NotImplementedException("Unhandled type.  Control should not make it here.");
        }

        public static Erroring<HalfType> Intersect(HalfType t, HalfType u)
        {
            if(t == u) return new Erroring<HalfType>(t);
            if (t == HalfType.AnyToAny) return new Erroring<HalfType>(u);
            if (u == HalfType.AnyToAny) return new Erroring<HalfType>(t);
            return new Erroring<HalfType>(GetName(t) + " has no intersection with " + GetName(u));
        }

        public static Erroring<HalfType> Compose(HalfType t, HalfType u)
        {
            switch (t)
            {
                case HalfType.SingleToSingle:
                    switch (u)
                    {
                        case HalfType.SingleToSingle: return new Erroring<HalfType>(HalfType.SingleToSingle);
                        case HalfType.SetToSingle: return new Erroring<HalfType>(HalfType.SingleToSingle);
                        case HalfType.AnyToAny: return new Erroring<HalfType>(HalfType.SingleToSingle);
                    } break;
                case HalfType.SetToSingle:
                    switch (u)
                    {
                        case HalfType.SingleToSingle: return new Erroring<HalfType>(HalfType.SetToSingle);
                        case HalfType.AnyToAny: return new Erroring<HalfType>(HalfType.SetToSingle);
                    } break;
                case HalfType.SetToSet:
                    switch (u)
                    {
                        case HalfType.SetToSingle: return new Erroring<HalfType>(HalfType.SetToSingle);
                        case HalfType.SetToSet: return new Erroring<HalfType>(HalfType.SetToSet);
                        case HalfType.AnyToAny: return new Erroring<HalfType>(HalfType.SetToSet);
                    } break;
                case HalfType.AnyToAny:
                    switch (u)
                    {
                        case HalfType.SingleToSingle: return new Erroring<HalfType>(HalfType.SingleToSingle);
                        case HalfType.SetToSingle: return new Erroring<HalfType>(HalfType.SetToSingle);
                        case HalfType.SetToSet: return new Erroring<HalfType>(HalfType.SetToSet);
                        case HalfType.AnyToAny: return new Erroring<HalfType>(HalfType.AnyToAny);
                    } break;
            }
            return new Erroring<HalfType>(GetName(t) + " does not compose with " + GetName(u));
        }
    }
}
