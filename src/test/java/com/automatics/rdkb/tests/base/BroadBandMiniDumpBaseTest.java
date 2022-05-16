/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.base;

import java.util.List;

import org.testng.annotations.AfterSuite;
import org.testng.annotations.BeforeMethod;

import com.automatics.constants.AutomaticsConstants;
import com.automatics.device.Dut;
import com.automatics.rack.RackInitializer;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Base class to perform all the pre and post operations for Google breakpad automation
 * 
 * @author Divya R S
 * @author Styles Mangalasseri
 * @refactor Anandam
 */
public class BroadBandMiniDumpBaseTest extends AutomaticsTestBase {

    /**
     * Verify whether STB is accessible
     * 
     * @param params
     *            Parameters to test method.
     */
    @BeforeMethod(alwaysRun = true)
    protected void beforeMethod(Object[] params) {
	if (null != params && params.length >= 1 && (params[0] instanceof Dut)) {
	    LOGGER.info("Verifying whether the given STB is accessible or not");
	    // current STB
	    Dut device = (Dut) params[0];
	    // wait for IP acquisition
	    CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	}
    }

    /**
     * Method to execute post conditions for all IP Linear test cases
     */
    @AfterSuite(alwaysRun = true)
    public void executeAfterSuite() {

	LOGGER.debug("Entering into executeAfterSuite()");

	List<Dut> lockedDevices = null;

	try {
	    // retrieve all the locked devices
	    lockedDevices = RackInitializer.getLockedSettops();

	    if (lockedDevices != null && lockedDevices.size() > 0) {
		for (Dut device : lockedDevices) {
		    // Reboot the devices. This reboot is introduced to coverup
		    // the issue in restarting the processes
		    LOGGER.info("Going to reboot the device " + device.getHostMacAddress());
		    tapEnv.rebootWithoutWait(device);
		}

		// After rebooting the devices waiting for 7 minutes to get the
		// box up.
		LOGGER.info("After rebooting the devices waiting for 7 minutes to get the box up.");
		tapEnv.waitTill(AutomaticsConstants.ONE_MINUTE * 7);
	    }
	} catch (Exception exception) {
	    LOGGER.error("Exception occured while rebooting the devices", exception);
	}

	LOGGER.debug("Exiting from executeAfterSuite()");
    }
}
