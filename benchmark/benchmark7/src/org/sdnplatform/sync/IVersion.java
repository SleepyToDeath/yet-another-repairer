/*
 * Copyright 2008-2009 LinkedIn, Inc
 * Copyright 2013 Big Switch Networks, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package org.sdnplatform.sync;

/**
 * An interface that allows us to determine if a given version happened before
 * or after another version.
 *
 * This could have been done using the comparable interface but that is
 * confusing, because the numeric codes are easily confused, and because
 * concurrent versions are not necessarily "equal" in the normal sense.
 *
 *
 */

public interface IVersion {
    /**
     * The result of comparing two times--either t1 is BEFORE t2,
     * t1 is AFTER t2, or t1 happens CONCURRENTLY to t2.
     */
    public enum Occurred {
        BEFORE,
        AFTER,
        CONCURRENTLY
    }

    /**
     * Return whether or not the given version preceeded this one, succeeded it,
     * or is concurrant with it
     *
     * @param v The other version
     */
    public Occurred compare(IVersion v);

}