using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    public class TypeCheck : MbNodeVisitor<Erroring<MbType>, Environment<MbType>>
    {

        public Erroring<MbType> ForCompose(Compose b, Environment<MbType> env)
        {
            return b.First.Accept(this, Environment<MbType>.EMPTY).Bind(
                t1 => b.Second.Accept(this, env).Bind(
                    t2 => MbType.Compose(t1.G, t2.G).Bind(
                        resG => MbType.Compose(t1.S, t2.S).FMap(
                            resS => new MbType(resG, resS)))));
        }

        public Erroring<MbType> ForFix(Fix b, Environment<MbType> arg1)
        {
            throw new NotImplementedException();
        }

        public Erroring<MbType> ForCall(Call b, Environment<MbType> arg1)
        {
            throw new NotImplementedException();
        }

        public Erroring<MbType> ForBranch(Branch b, Environment<MbType> arg1)
        {
            throw new NotImplementedException();
        }
    }
}
