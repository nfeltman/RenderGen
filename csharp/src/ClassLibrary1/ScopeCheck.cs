using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    class ScopeCheck : MbNodeVisitor<Erroring<Unit>, Environment<Unit>>
    {
        public Erroring<Unit> ForCompose(Compose b, Environment<Unit> e)
        {
            return b.Accept(this, Environment<Unit>.EMPTY).Bind(() => b.Accept(this, e));
        }

        public Erroring<Unit> ForFix(Fix b, Environment<Unit> e)
        {
            return b.Accept(this, e.Add(b.Label, Unit.ONLY));
        }

        public Erroring<Unit> ForCall(Call b, Environment<Unit> e)
        {
            return e.LookUp(b.Label);
        }

        public Erroring<Unit> ForBranch(Branch b, Environment<Unit> e)
        {
            return b.Left.Accept(this, e).Bind(() => b.Right.Accept(this, e));
        }
    }
}
