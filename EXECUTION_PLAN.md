# Crystal V2 - Execution Plan (Consensus)

**Decision Date:** 2025-11-01
**Method:** Maieutic analysis (Socrates, Daedalus, Cassandra)
**Consensus:** Path D (Hybrid) - 80% probability of success

---

## Strategic Insight

**Socratic Truth:** LSP without solid type inference = poor DX
**Daedalus Observation:** We already have 70% of type inference working
**Cassandra Prediction:** Path D has 80% success probability vs 40-65% for alternatives

**Key Realization:** We're closer than we thought. 2 weeks to solid foundation → 2 weeks to great LSP.

---

## The Plan: Foundation First, Then Speed

### Week 1: Generic Type Instantiation (CRITICAL)

**Goal:** Generic types work reliably

**Success Criteria:**
```crystal
# Must work:
def identity(x : T) forall T
  x
end

identity(42)      # → Int32 ✅
identity("hello") # → String ✅

class Box(T)
  def initialize(@value : T); end
  def get : T; @value; end
end

box = Box.new(42)
box.get  # → Int32 ✅
```

**Tasks:**
1. Generic method type parameter binding
2. Generic class instantiation
3. Type parameter propagation through calls
4. Basic constraint checking (T < Number, etc.)

**Deliverable:** Generics work for 80% of common cases

**Estimated:** 5-7 days full focus

---

### Week 2: Union Types & Type Narrowing (CRITICAL)

**Goal:** Type narrowing works for LSP hover

**Success Criteria:**
```crystal
# Must work:
x : Int32 | String = get_value

if x.is_a?(Int32)
  # LSP hover here shows: x is Int32 (not Int32 | String)
  x + 1  # ✅ No error
else
  # LSP hover here shows: x is String
  x.upcase  # ✅ No error
end

# Also:
y : Int32? = nil
if y
  y + 1  # y is Int32 here (not nilable)
end
```

**Tasks:**
1. Union type creation from if/else branches
2. Type narrowing with is_a?
3. Type narrowing with nil check
4. Type narrowing with responds_to?
5. Scope-aware type tracking

**Deliverable:** Type narrowing works in LSP hover

**Estimated:** 5-7 days full focus

---

### Week 3: LSP MVP (INTEGRATION)

**Goal:** Working LSP using existing 70% + Week 1-2 improvements

**Success Criteria:**
- `crystal tool lsp` command works
- VS Code connects successfully
- Syntax errors show in real-time
- Hover shows types for ~80% of code
- Go-to-definition works for methods/classes
- Response time < 100ms (target: 50ms)

**Tasks:**
1. LSP protocol handler (JSON-RPC over stdio)
2. Document management (didOpen, didChange, didClose)
3. Diagnostics publishing (syntax + basic semantic)
4. Hover implementation (using TypeInferenceEngine)
5. Go-to-definition (using SymbolTable)
6. Basic testing with VS Code

**Deliverable:** LSP MVP that's actually useful

**Estimated:** 5-7 days

---

### Week 4: LSP Advanced + Polish (QUALITY)

**Goal:** Production-ready LSP

**Success Criteria:**
- Completion works (shows methods from inferred types)
- Find references works
- Response time < 50ms
- Incremental updates work (VirtualArena.replace_file_arena)
- No crashes on invalid code
- Memory efficient (doesn't leak on edits)

**Tasks:**
1. Completion implementation
2. Find references implementation
3. Performance optimization
4. Incremental update testing
5. Error recovery hardening
6. Memory leak testing

**Deliverable:** LSP ready for beta

**Estimated:** 5-7 days

---

### Week 5: Beta Release & Integration (LAUNCH)

**Goal:** Public beta, gather feedback

**Success Criteria:**
- VS Code extension published
- Documentation complete
- At least 10 beta testers
- Performance benchmarks published
- GitHub issues triaged

**Tasks:**
1. VS Code extension package
2. Write documentation (installation, usage)
3. Create demo videos
4. Beta announcement (Crystal forum, Reddit)
5. Set up issue templates
6. Monitor and fix critical bugs

**Deliverable:** Public beta with feedback loop

**Estimated:** 3-5 days

---

### Week 6+: CrystalGuard (PARALLEL TRACK)

**Goal:** Unique competitive advantage

**Why parallel:** We'll be dogfooding LSP, can work on security tool simultaneously

**Success Criteria:**
```bash
$ crystal tool guard src/

🔴 CRITICAL (2)
  src/api/auth.cr:42:15
  Hardcoded API key detected

  src/db/queries.cr:15:8
  SQL injection via string interpolation

Scan completed in 0.234s
```

**Tasks:**
1. Secrets detection (regex patterns)
2. SQL injection detection (AST pattern matching)
3. Command injection detection
4. SARIF output format
5. CI/CD integration guide

**Deliverable:** `crystal tool guard` command

**Estimated:** 1-2 weeks (can be done by different person)

---

## Success Metrics

### Week 1-2 (Type Inference):
- ✅ Generics work for common cases
- ✅ Union narrowing works in control flow
- ✅ Pass 50+ new type inference tests

### Week 3-4 (LSP):
- ✅ < 50ms latency for hover/goto
- ✅ ~80% type coverage in hover
- ✅ Works on real projects (Kemal, Lucky)
- ✅ Zero crashes in 1 hour usage

### Week 5 (Beta):
- ✅ 10+ beta testers
- ✅ 3+ positive reviews
- ✅ Published VS Code extension
- ✅ 50+ GitHub stars (target)

### Week 6+ (CrystalGuard):
- ✅ Detects secrets in popular shards
- ✅ Finds real vulnerabilities
- ✅ < 1s scan time for typical project

---

## Risk Mitigation

### Risk 1: Type Inference Takes Longer
**Probability:** 30%
**Impact:** High (delays LSP)

**Mitigation:**
- Daily progress tracking
- If stuck >2 days on one issue → DAEDALUS-MAIEUTIC analysis
- Fallback: Ship LSP with partial types, iterate

### Risk 2: LSP Performance Issues
**Probability:** 20%
**Impact:** Medium (users complain)

**Mitigation:**
- VirtualArena already optimized (O(log N) lookup)
- Profile early (Week 3 day 1)
- Target < 100ms initially, optimize to < 50ms later

### Risk 3: Community Doesn't Adopt
**Probability:** 15%
**Impact:** High (wasted effort)

**Mitigation:**
- Beta with influential community members
- Demo videos showing speed advantage
- Compare with Crystalline (show 10x faster)
- Active support on Discord/Forum

### Risk 4: Burnout / Lost Motivation
**Probability:** 25%
**Impact:** Critical (project stops)

**Mitigation:**
- Celebrate small wins (end of each week)
- Public progress updates (maintain excitement)
- Parallel CrystalGuard track (variety)
- Community engagement (external motivation)

---

## Why This Plan Works

### 1. Solid Foundation
**Weeks 1-2 on inference = LSP built on rock, not sand**

Without generics + unions working:
- Hover shows "Unknown" everywhere
- Completion is useless
- Users disappointed

With generics + unions working:
- Hover shows real types
- Completion suggests real methods
- Users impressed

### 2. Visible Progress
**Every week has a demo-able milestone**

- Week 1: "Generics work! Look at this Box(T) example"
- Week 2: "Type narrowing! Hover in if-branch shows narrowed type"
- Week 3: "LSP works! Real-time errors in VS Code"
- Week 4: "Completion! See methods appear as I type"
- Week 5: "Beta release! Try it yourself"

### 3. Competitive Differentiation
**CrystalGuard = unique value**

No other Crystal tool does security analysis:
- Ameba: Style only
- Crystalline: LSP only (and slow)
- Crystal compiler: No static security checks

Crystal V2 stack:
- ✅ Fast LSP (< 50ms)
- ✅ Security analysis (CrystalGuard)
- ✅ Fast compilation (future)

= **Complete developer experience**

### 4. High Success Probability
**80% vs 40-65% for alternatives**

Path A (LSP first): 40% - Poor first impression
Path B (Inference first): 65% - No visible progress early
Path C (True parallel): 55% - Context switching fatigue
**Path D (Hybrid): 80% - Best of both worlds**

---

## Execution Philosophy

### Week 1-2: DEEP FOCUS
- No LSP work
- No distractions
- 100% on type inference
- Goal: Solid foundation

### Week 3-4: INTEGRATION
- Combine inference + LSP
- Iterate rapidly
- Daily testing
- Goal: Working product

### Week 5: LAUNCH
- Community focus
- Documentation
- Support
- Goal: Adoption

### Week 6+: EXPANSION
- CrystalGuard
- Dogfooding LSP
- Fix bugs from beta
- Goal: Completeness

---

## Next Actions (Week 1 - Day 1)

### Today:
1. ✅ Create this execution plan
2. ⏭️ Set up type inference tests (generic_spec.cr)
3. ⏭️ Analyze current generic implementation gaps
4. ⏭️ Design generic type instantiation algorithm
5. ⏭️ Start implementation

### This Week:
- Daily standup (track progress)
- End of week review (is generics working?)
- Adjust plan if needed

---

## Commit to Excellence

**We're not building "another LSP"**
**We're building the LSP Crystal deserves**

**We're not rushing to ship**
**We're building a foundation that lasts**

**We're not following Go**
**We're learning from Go's success and applying it to a superior language**

---

**Status:** Ready to execute
**Confidence:** 80%
**Timeline:** 6 weeks to beta + security tool
**Goal:** Make Crystal compilation as fast as Go, while keeping Crystal's beautiful language design

🚀 Let's build the future of Crystal development.
