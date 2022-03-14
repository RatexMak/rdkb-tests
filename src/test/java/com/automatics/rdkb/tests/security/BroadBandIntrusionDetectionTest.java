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
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandIntrusionDetectionTest extends AutomaticsTestBase {
    /**
     * Verify lighttpd updated to the latest stable version
     * <ol>
     * <li>Verify the updated lighttpd version in the device.</li>
     * <li>Verify that lighty config (/var/lighttpd.conf) does not contain the message "url-path-2f-decode" =>
     * "enable"</li>
     * 
     * @author Dipankar Nalui
     * @refactor Govardhan
     *           </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-LIGHTTPD-1002")
    public void testToVerifyLighttpdVersion(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-LIGHTTPD-102";
	String stepNum = "s1";
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-LIGHTTPD-1002");
	LOGGER.info("TEST DESCRIPTION: Verify lighttpd updated to the latest stable version");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the updated lighttpd version in the device.");
	LOGGER.info(
		"2. Verify that lighty config (/etc/lighttpd.conf ) does not contain the message \"url-path-2f-decode\" => \"enable\"");

	LOGGER.info("#######################################################################################");

	stepNum = "s1";
	status = false;
	String versionInStbProperties = BroadbandPropertyFileHandler.getLighttpdVersionFromProperties();
	errorMessage = "updated lighttpd version in the device is not " + versionInStbProperties;

	try {
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the updated lighttpd version in the device.");
	    LOGGER.info("STEP 1: ACTION : Execute the following command: /usr/sbin/lighttpd -version");
	    LOGGER.info(
		    "STEP 1: EXPECTED : updated lighttpd version in the device should be " + versionInStbProperties);
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.verifyLighttpdVersion(device, tapEnv, versionInStbProperties);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : updated lighttpd version in the device is " + versionInStbProperties);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "lighty config (/etc/lighttpd.conf) contains the message \"url-path-2f-decode\" => \"enable\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify that lighty config (/etc/lighttpd.conf) does not contain the message \"url-path-2f-decode\" => \"enable\"");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the following command:1. grep -i \"url-path-2f-decode\" /etc/lighttpd.conf");
	    LOGGER.info(
		    "STEP 2: EXPECTED : lighty config (/etc/lighttpd.conf ) should not contain the message \"url-path-2f-decode\" => \"enable\"");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SERVER_HTTP_PARSEOPTS,
		    BroadBandTestConstants.CONSTANT_LIGHTTPD_CONF, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response.trim().toLowerCase(),
			    BroadBandTraceConstants.LOG_MESSAGE_URL_PATH_2F_DECODE.toLowerCase())
		    && !CommonUtils.isGivenStringAvailableInCommandOutput(response.trim().toLowerCase(),
			    BroadBandTestConstants.STRING_ENABLE.toLowerCase());

	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : lighty config (/var/lighttpd.conf) does not contain the message \"url-path-2f-decode\" => \"enable\"");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-LIGHTTPD-1002");
    }
}
