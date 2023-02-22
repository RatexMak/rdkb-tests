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

package com.automatics.rdkb.tests.security;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;

/**
 * Test class written as a part of automation testing of RDKB new features related to Security.
 * 
 * @author BALAJI V
 * @refactor Govardhan
 */

public class BroadBandSecurityLibraryTest extends AutomaticsTestBase {

    /**
     * 
     * <ol>
     * <li>STEP 1: Verify whether the telnet is removed from the build
     * <li>EXPECTED: The returned value should be 'No such file or directory'
     * 
     * <li>STEP 2: Verify whether the rlogin is removed from the build
     * <li>EXPECTED: The returned value should be 'No such file or directory'
     * 
     * <li>STEP 3: Verify whether the NFS is removed from the build
     * <li>EXPECTED: The returned value should be 'No such file or directory'
     * </ol>
     * 
     * @param device
     *            The Dut to be used.
     * @author BALAJI V
     * @refactor Govardhan
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SECURITY-5001")
    public void telnetRloginNfsLibraryTest(Dut device) {
	String testCaseId = "TC-RDKB-SECURITY-501";
	boolean result = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    /**
	     * STEP 1: Verify the telnet library is not available on the device.
	     */
	    LOGGER.info("STEP 1: VERIFY TELNET LIBRARY IS NOT AVAILABLE.");
	    LOGGER.info("STEP 1: EXPECTED - TELNET LIBRARY MUST NOT BE AVAILABLE.");
	    result = !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device, BroadBandTestConstants.LIB_TELNET);
	    errorMessage = "TELNET LIBRARY IS PRESENT ON THE DEVICE.";
	    LOGGER.info("STEP 1: ACTUAL: " + (result ? "TELNET LIBRARY IS NOT PRESENT." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * STEP 2: Verify the rlogin library is not available on the device.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info("STEP 2: VERIFY RLOGIN LIBRARY IS NOT AVAILABLE.");
	    LOGGER.info("STEP 2: EXPECTED - RLOGIN LIBRARY MUST NOT BE AVAILABLE.");
	    result = !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device, BroadBandTestConstants.LIB_RLOGIN);
	    errorMessage = "RLOGIN LIBRARY IS PRESENT ON THE DEVICE.";
	    LOGGER.info("STEP 2: - ACTUAL: " + (result ? "RLOGIN LIBRARY IS NOT PRESENT." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * STEP 3: Verify the nfs library is not available on the device.
	     */
	    step = "s3";
		result = false;
		LOGGER.info("STEP 3: VERIFY NFS LIBRARY IS NOT AVAILABLE.");
		LOGGER.info("STEP 3: - EXPECTED - NFS LIBRARY MUST NOT BE AVAILABLE.");
		if (!DeviceModeHandler.isRPIDevice(device)) {
			result = !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device, BroadBandTestConstants.LIB_NFS);
			errorMessage = "NFS LIBRARY IS PRESENT ON THE DEVICE.";
			LOGGER.info("STEP 3: - ACTUAL: " + (result ? "NFS LIBRARY IS NOT PRESENT." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
		} else {
			LOGGER.info("NFS is available in RPi : skipping teststep ...");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
		}
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE SECURITY - REMOVAL OF TELNET, RLOGIN, NFS LIBRARIES: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	}
    }
}